#ifndef __HLFS_SNAPSHOT_HELPER_H_
#define __HLFS_SNAPSHOT_HELPER_H_

#include <stdint.h>
#include <hlfs_ctrl.h>
#include <stdio.h>
#include "snapshot.h"

int snapshot2text(const struct snapshot* snapshot, char*textbuf);
int dump_snapshot(struct back_storage *storage,const char* snapshot_file,struct snapshot * snapshot);
int snapshot_delmark2text(const char* ssname, char*textbuf);
int dump_snapshot_delmark(struct back_storage *storage,const char* snapshot_file,const char* ssnamea);
int load_snapshot_from_text(struct back_storage *storage,const char* snapshot_file,const char* snapshot_textbuf);
#endif 
