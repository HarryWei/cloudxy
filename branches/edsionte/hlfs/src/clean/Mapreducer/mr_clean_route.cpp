/*
  *  Copyright (C) 2012 KangHua <kanghua151@gmail.com>
  *
  *  This program is free software; you can redistribute it and/or modify it
  *  under the terms of the GNU General Public License version 2 as published by
  *  the Free Software Foundation.
 */

#include "Pipes.hh"
#include "TemplateFactory.hh"
#include "StringUtils.hh"
#include "SerialUtils.hh"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>
#include <stdint.h>
#include <iostream>
#include <string>
#include "hdfs.h"
#include "glib.h"
#include "storage_helper.h"
#include "seg_clean.h"
#include "seg_clean_helper.h"
#include "snapshot_helper.h"
#include "snapshot.h"
#include "hlfs_ctrl.h"
#include "misc.h"

#define Swap16(s) ((((s) & 0xff) << 8) | (((s) >> 8) & 0xff)) 
#define Swap32(l) (((l) >> 24) | \  
        (((l) & 0x00ff0000) >> 8)  | \  
        (((l) & 0x0000ff00) << 8)  | \  
        ((l) << 24)) 

#define Swap64(ll) (((ll) >> 56) |\  
        (((ll) & 0x00ff000000000000) >> 40) |\  
        (((ll) & 0x0000ff0000000000) >> 24) |\  
        (((ll) & 0x000000ff00000000) >> 8)    |\  
        (((ll) & 0x00000000ff000000) << 8)    |\  
        (((ll) & 0x0000000000ff0000) << 24) |\  
        (((ll) & 0x000000000000ff00) << 40) |\  
        (((ll) << 56)))


using namespace std;

class SegUsageCalcMap: public HadoopPipes::Mapper {
    public:
        uint32_t m_block_size;
	 uint32_t m_segment_size;
        //struct inode* m_latest_inode;
        struct back_storage * m_storage; 
        SegUsageCalcMap(HadoopPipes::TaskContext& context) {
            //inputWords = context.getCounter(WORDCOUNT, INPUT_WORDS);
            const HadoopPipes::JobConf* job = context.getJobConf();
            std::string inputDir = job->get("mapred.input.dir");
            const char * uri = inputDir.c_str();
            const char * fs_name = g_basename(inputDir.c_str());
	     printf("DBG:--input Dir:%s,fs_name:%s,uri:%s\n",inputDir.c_str(),fs_name,uri);
            m_storage = init_storage_handler(uri);
            HADOOP_ASSERT(m_storage != NULL, "failed to init storage handler ");
            uint32_t segment_size;
            uint32_t block_size;
            uint64_t max_fs_size;
            int ret = read_fs_meta(m_storage, &segment_size, &block_size,&max_fs_size);
            printf("DBG:--segment size:%u,block size:%u,max fs size%llu\n",segment_size,block_size,max_fs_size);
            //m_latest_inode = load_latest_inode(m_storage); 
     	    //HADOOP_ASSERT(m_latest_inode != NULL, "failed to load latest inode ");
            m_block_size = block_size;
        }            
        
        /* 
         * 1.exec segment usage calc
         * 2.emit (segno,seg_usage_text)
         */
        void map(HadoopPipes::MapContext& context) {
              int ret = 0;
		printf("DBG:-- enter func:%s\n",__func__);
		const char *segfile = context.getInputKey().data();
		printf("DBG:-- key len :%d ,segfile:%s\n",context.getInputValue().size(),segfile);
		uint64_t segno = get_segfile_no(segfile);
     	       HADOOP_ASSERT(segfile != NULL, "failed read segfile ");
			   
              GHashTable   *ss_hashtable = g_hash_table_new_full(g_str_hash,g_str_equal,NULL,NULL);
              ret = load_all_snapshot(m_storage,SNAPSHOT_FILE,ss_hashtable);
              printf("DBG:-- snapshot loaded\n"); 
              g_assert(ret == 0);
              GList* ss_list = NULL;
              ret = sort_all_snapshot(ss_hashtable,&ss_list);
              printf("DBG:--snapshot sorted\n"); 
              g_assert(ss_list !=NULL);
              g_assert(ret == 0);
              //struct inode * latest_inode = load_latest_inode(storage); 
              struct inode * inode=NULL;
              char *up_sname;
              ret = get_refer_inode_between_snapshots(m_storage,segno,ss_list,&inode,&up_sname);
		SEG_USAGE_T seg_usage;
		memset(&seg_usage,0,sizeof(SEG_USAGE_T));
		if(ret == 0){
                  printf("DBG:--seg is in snapshots\n");
                  strncpy(seg_usage.up_sname,up_sname,strlen(up_sname));
                  ret  = seg_usage_calc(m_storage,m_block_size,segno,inode,&seg_usage);
                  printf("up sname is:%s\n",seg_usage.up_sname);
                  g_assert(ret ==0);   
              }
              if(ret == 1){
                  printf("DBG:--seg is on snapshot,do nothing\n");
              }
              if(ret == 2){
                  printf("DBG:--seg is above snapshot,maybe need migrate\n");
                  strncpy(seg_usage.up_sname,EMPTY_UP_SNAPSHOT,strlen(EMPTY_UP_SNAPSHOT));
                  printf("DBG:--up sname is:%s\n",seg_usage.up_sname);
		    inode = load_latest_inode(m_storage); 
                  ret     =  seg_usage_calc(m_storage,m_block_size,segno,inode,&seg_usage);
                  g_assert(ret ==0);
              }
#if 1


             string key =string(segfile,strlen(segfile));
             char segtextbuf[4096];
             uint32_t len = seg_usage2text(&seg_usage,segtextbuf);
	      printf("DBG:--segtextbuf :%s ..\n",segtextbuf);
             string value = string(segtextbuf,len);
	      printf("DBG:--send segment usage text to reducer ..\n");
             context.emit(key,value);
#endif 
             g_free(seg_usage.bitmap);

	}

        ~SegUsageCalcMap() {
		deinit_storage_handler(m_storage);
	}
};

class SegUsageCalcReduce: public HadoopPipes::Reducer {
public:
  //HadoopPipes::TaskContext::Counter* outputWords;
  SegUsageCalcReduce(HadoopPipes::TaskContext& context) {
    printf("GDB:--enter func%s\n",__func__); 
  }
  void reduce(HadoopPipes::ReduceContext& context) {
    printf("DBG:--enter reduce func ...\n");
    while(context.nextValue()){
          printf("DBG:--key:%s value%s\n",context.getInputKey().c_str(),context.getInputValue().c_str());
    }
    context.emit(context.getInputKey(),context.getInputValue());
    printf("DBG:--exit reduce func ...\n");
  }
};

/* we need seg file name only ,so read a new recordreader for it  */
class SegUsageCalcReader: public HadoopPipes::RecordReader {
private:
  int64_t m_bytes_total;
  int64_t m_bytes_read;
  const char* m_seg_file;
public:
  
  SegUsageCalcReader(HadoopPipes::MapContext& context) {
	std::string _filename; 
	/* FIXIT : hardcore for get segfile name from hadoop proctocol ? */
	int16_t mysize = *(int16_t*)context.getInputSplit().data();
	_filename = context.getInputSplit().data()+2;
	printf("GDB:-- filename :%s sizeof:%d\n",_filename.c_str(),_filename.size());
	uint64_t _offset = *(int64_t*)(context.getInputSplit().data()+ 2 +_filename.size()); 
	uint64_t offset = Swap64(_offset);
	uint64_t _len = *(int64_t*)(context.getInputSplit().data()+2+_filename.size()+8); 
	uint64_t len = Swap64(_len);
	printf("GDB:-- seg offset:%lld len:%lld\n",offset,len);
       std::string filename = _filename.data()+5;
	printf("GDB:-- filename :%s sizeof:%d\n",filename.c_str(),filename.size());
	if(TRUE!=g_str_has_suffix(filename.c_str(),"seg")){
		printf("GDB:-- ignore it \n");
		m_bytes_total = m_bytes_read = 0;
		return;
	}
	m_seg_file = g_strdup(g_basename(filename.c_str()));
	printf("GDB:-- seg file:%s\n",m_seg_file);
       m_bytes_total = len;
       m_bytes_read = 0;
  }

  /*
   * only need extract segfile name as key by one segfile
   */
  virtual bool next(std::string& key, std::string& value) {
    printf("GDB:-- enter func:%s\n",__func__);
    if(m_bytes_total==m_bytes_read){
        return false;
    }
    key =string(m_seg_file,strlen(m_seg_file));
    printf("GDB:-- segfile:%s\n",m_seg_file);
    value = "";
    m_bytes_read += m_bytes_total;
    return true;
  }

  /**
   * The progress of the record reader through the split as a value between
   * 0.0 and 1.0.
   */
  virtual float getProgress() {
    if (m_bytes_read > 0) {
      return (float)m_bytes_read/m_bytes_total;
    } else {
      return 1.0f;
    }
  }
 
};

/* we need write all seg usage text to a SEGMENTS_USAGE_FILE */
class SegUsageCalcWriter: public HadoopPipes::RecordWriter {
private:

public:
    struct back_storage * m_storage; 
    SegUsageCalcWriter(HadoopPipes::ReduceContext& context) {
        printf("DBG:--enter func%s\n",__func__); 
        const HadoopPipes::JobConf* job = context.getJobConf();
        int part = job->getInt("mapred.task.partition");
        std::string outDir = job->get("mapred.work.output.dir");
        std::string inputDir = job->get("mapred.input.dir");
        printf("DBG:--inputDir :%s\n",inputDir.c_str());
        const char * fs_name = g_basename(inputDir.c_str());
        const char * uri = inputDir.c_str();
        printf("DBG:--fs_name:%s,uri:%s\n",fs_name,uri);
        m_storage = init_storage_handler(uri);
        HADOOP_ASSERT(m_storage != NULL, "failed to init storage handler ");
    }
    ~SegUsageCalcWriter() {
	 deinit_storage_handler(m_storage);
    }

    void emit(const std::string& key, const std::string& value) {
        printf("DBG:--enter recordwriter's emit\n");
        const char *segtextbuf = value.c_str();
        printf("DBG:--segment usage text :%s\n",segtextbuf);
        //dump_seg_usage_text(m_storage,SEGMENTS_USAGE_FILE,segtextbuf);
        size_t len = value.size();
        int  ret = file_append_contents(m_storage,SEGMENTS_USAGE_FILE,segtextbuf,len);
        g_assert(ret == 0);
    }
};

int main(int argc, char *argv[]){ 
  return HadoopPipes::runTask(HadoopPipes::TemplateFactory<SegUsageCalcMap, 
                              SegUsageCalcReduce, void, void, SegUsageCalcReader,
                              SegUsageCalcWriter>());
}
