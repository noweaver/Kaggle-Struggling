#!/usr/bin/env python
# -*- coding:utf-8 -*-
__author__ = 'Ryan Guhnguk Ahn'


import sys
import os
import glob
import commands
import logging

logging.basicConfig(filename='./upload2db.log', level=logging.DEBUG)

console = logging.StreamHandler()
console.setLevel(logging.DEBUG)

logging.getLogger('').addHandler(console)


def build_dbscript(entry_path):
    cmd_head = "hive -e \"load data local inpath \'"
    cmd_tail = "\' into table kaggle.driver_telematics_analysis\""

    for dir_path in glob.glob(entry_path + "/*"):
        if os.path.isdir(dir_path):
            for f in glob.glob(dir_path + "/*"):
                cmd = cmd_head + f + cmd_tail
                logging.info(cmd)
                result = commands.getoutput(cmd)
                logging.info(result)
                

if __name__ == "__main__":
    #sys.argv = [r"D:\Workspace\Data\Kaggle\Driver_Telematics_Analysis\result-drivers"]
    dir_path = sys.argv
    #print dir_path[0]
    #print dir_path[1]

    build_dbscript(dir_path[0])
