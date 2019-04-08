#!/usr/bin/env python
# -*- coding:utf-8 -*-
__author__ = 'Ryan Guhnguk Ahn'


import sys
import os
import glob
import logging

logging.basicConfig(filename='./line_count.log', level=logging.DEBUG)

console = logging.StreamHandler()
console.setLevel(logging.DEBUG)

# formatter = logging.Formatter('%(name)-12s: %(levelname)-8s %(message)s')
# tell the handler to use this format
# console.setFormatter(formatter)
# add the handler to the root logger
logging.getLogger('').addHandler(console)


def line_count(entry_path):
    if os.path.isfile(entry_path):
        cnt = len([line for line in open(entry_path, "r").read()[1:].splitlines() if line])
        logging.info(os.path.abspath(entry_path) + ", count => " + str(cnt))

    else:
        dir_count = 0
        total_count = 0
        for read_dir in glob.glob(entry_path + '/*'):
            if os.path.isfile(read_dir):
                cnt = len([line for line in open(read_dir, "r").read()[1:].splitlines() if line])
                logging.info(os.path.abspath(read_dir) + ", count => " + str(cnt))
                total_count += cnt
            else:
                for f in glob.glob(read_dir + '/*'):
                    names_list = [line for line in open(f, "r").read()[1:].splitlines() if line]
                    cnt = len(names_list)

                    dir_count += cnt
                    # logging.info(os.path.abspath(f) + ", " + str(cnt))
                logging.info(os.path.abspath(read_dir) + ", count => " + str(dir_count))

            total_count += dir_count
            dir_count = 0
        logging.info(os.path.abspath(entry_path) + ", total count => " + str(total_count))


if __name__ == "__main__":
    sys.argv = [r"D:\Workspace\Data\Kaggle\Driver_Telematics_Analysis\result-drivers\1\1.new"]
    #sys.argv = [r"D:\Workspace\Data\Kaggle\Driver_Telematics_Analysis\drivers"]
    dir_path = sys.argv
    #print dir_path[0]
    #print dir_path[1]

    line_count(dir_path[0])
