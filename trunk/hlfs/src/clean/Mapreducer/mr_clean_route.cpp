/**
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
#include "segment_cleaner.h"
#include "hlfs_ctrl.h"

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

//const std::string INPUT_WORDS = "INPUT_WORDS";
//const std::string OUTPUT_WORDS = "OUTPUT_WORDS";
//const std::string WORDCOUNT = "WORDCOUNT";
using namespace std;

class SegUsageCalcMap: public HadoopPipes::Mapper {
    public:
        uint32_t m_block_size;
	    uint32_t m_segment_size;
        struct inode* m_latest_inode;
        struct back_storage * m_storage; 
        SegUsageCalcMap(HadoopPipes::TaskContext& context) {
            //inputWords = context.getCounter(WORDCOUNT, INPUT_WORDS);
            const HadoopPipes::JobConf* job = context.getJobConf();
            std::string inputDir = job->get("mapred.input.dir");
            printf("DBG:--inputDir :%s\n",inputDir.c_str());
            const char * fs_name = g_basename(inputDir.c_str());
            const char * uri = g_dirname(inputDir.c_str());
	        printf("DBG:--fs_name:%s,uri:%s\n",fs_name,uri);
            m_storage = init_storage_handler(uri,fs_name);
            HADOOP_ASSERT(m_storage != NULL, "failed to init storage handler ");
            uint32_t segment_size;
            uint32_t block_size;
            int ret = read_fs_meta(m_storage, &segment_size, &block_size);
            printf("DBG:--segment size:%u,block size:%u\n",segment_size,block_size);
            SEGMENT_SIZE_MASK  = segment_size - 1;
            SEGMENT_SIZE_SHIFT = 0;
            while (0 != (segment_size = (segment_size >> 1)))
            {                                                                                                         
                SEGMENT_SIZE_SHIFT++;
            }
            m_latest_inode = load_latest_inode(m_storage); 
     	    HADOOP_ASSERT(m_latest_inode != NULL, "failed to load latest inode ");
            m_block_size = block_size;
        }            
        
        /* 1 exec segment usage calc
         * 2 emit (segno,seg_usage_text)
         */
        void map(HadoopPipes::MapContext& context) {
		printf("DBG:-- enter func:%s\n",__func__);
		struct segment_usage seg_usage; 
		memset(&seg_usage,0,sizeof(struct segment_usage));
		const char *segfile = context.getInputKey().data();
		printf("DBG:-- key len :%d ,segfile:%s\n",context.getInputValue().size(),segfile);
     	HADOOP_ASSERT(segfile != NULL, "failed read segfile ");
		segment_usage_calc(m_storage,segfile,m_latest_inode,&seg_usage,m_block_size);
#if 0
		gchar * segusage_file = g_strconcat(segfile,".usage",NULL);
		dump_segment_usage(m_storage,segusage_file,&seg_usage);
		g_free(segusage_file);
		g_free(seg_usage.bitmap);
#endif 
        
#if 1


        string key =string(segfile,strlen(segfile));
        char segtextbuf[sizeof(struct segment_usage)*10];
        uint32_t len = segment_usage2text(&seg_usage,segtextbuf);
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
  HadoopPipes::TaskContext::Counter* outputWords;
  SegUsageCalcReduce(HadoopPipes::TaskContext& context) {
    //outputWords = context.getCounter(WORDCOUNT, OUTPUT_WORDS);
    printf("GDB:--enter func%s\n",__func__); 
  }
  void reduce(HadoopPipes::ReduceContext& context) {
    //int sum = 0;
    //while (context.nextValue()) {
    //  sum += HadoopUtils::toInt(context.getInputValue());
    //}y
    
    printf("DBG:--enter reduce func ...\n");
    while(context.nextValue()){
          printf("DBG:--key:%s value%s\n",context.getInputKey().c_str(),context.getInputValue().c_str());
    }
    context.emit(context.getInputKey(),context.getInputValue());
    printf("DBG:--exit reduce func ...\n");
    //context.incrementCounter(outputWords, 1); 
    //TODO
  }
};

class SegUsageCalcReader: public HadoopPipes::RecordReader {
private:
  int64_t m_bytes_total;
  int64_t m_bytes_read;
  const char* m_seg_file;
public:
  
  SegUsageCalcReader(HadoopPipes::MapContext& context) {
	std::string _filename; 
	int16_t mysize = *(int16_t*)context.getInputSplit().data();
	//printf("GDB:-- context2 size:%d\n",Swap16(mysize));
	_filename = context.getInputSplit().data()+2;
	printf("GDB:-- filename :%s sizeof:%d\n",_filename.c_str(),_filename.size());
	uint64_t _offset = *(int64_t*)(context.getInputSplit().data()+ 2 +_filename.size()); 
	uint64_t offset = Swap64(_offset);
	uint64_t _len = *(int64_t*)(context.getInputSplit().data()+2+_filename.size()+8); 
	uint64_t len = Swap64(_len);
	printf("GDB:-- regon offset:%lld len:%lld\n",offset,len);
        //std::string filename = _filename.substr(0,_filename.size()-16);
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


class SegUsageCalcWriter: public HadoopPipes::RecordWriter {
private:

public:
    struct back_storage * m_storage; 
    SegUsageCalcWriter(HadoopPipes::ReduceContext& context) {
        printf("GDB:--enter func%s\n",__func__); 
        const HadoopPipes::JobConf* job = context.getJobConf();
        int part = job->getInt("mapred.task.partition");
        std::string outDir = job->get("mapred.work.output.dir");
        std::string inputDir = job->get("mapred.input.dir");
        printf("DBG:--inputDir :%s\n",inputDir.c_str());
        const char * fs_name = g_basename(inputDir.c_str());
        const char * uri = g_dirname(inputDir.c_str());
        printf("DBG:--fs_name:%s,uri:%s\n",fs_name,uri);
        m_storage = init_storage_handler(uri,fs_name);
        HADOOP_ASSERT(m_storage != NULL, "failed to init storage handler ");
    }
    ~SegUsageCalcWriter() {
		deinit_storage_handler(m_storage);
    }

  void emit(const std::string& key, const std::string& value) {
       printf("DBG:--enter recordwriter's emit\n");
       const char *segtextbuf = value.c_str();
       printf("DBG:--segment usage text :%s\n",segtextbuf);
       dump_segment_usage_text(m_storage,SEGMENTS_USAGE_FILE,segtextbuf);
  }
};

int main(int argc, char *argv[]){ 
  return HadoopPipes::runTask(HadoopPipes::TemplateFactory<SegUsageCalcMap, 
                              SegUsageCalcReduce, void, void, SegUsageCalcReader,
                              SegUsageCalcWriter>());
}
