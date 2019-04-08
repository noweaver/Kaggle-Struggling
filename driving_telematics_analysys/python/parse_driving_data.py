#!/usr/bin/env python
# -*- coding:utf-8 -*-
__author__ = 'Ryan Guhnguk Ahn'


import sys
import os
import glob
import logging


from math import *


logging.basicConfig(filename='./parse_driving_data.log', level=logging.DEBUG)


FILE_SIZE = 1024 * 1024 * 128


def basic_features(before_pt, current_pt):
    diff_x = float(before_pt[0]) - float(current_pt[0])
    diff_y = float(before_pt[1]) - float(current_pt[1])

    distance = math.sqrt(math.pow(diff_x, 2) + math.pow(diff_y, 2))
    velocity = distance * 3600 / 1000

    return distance, velocity


def parse_data(src_path, tar_path):
    record = ""
    file_count = 0

    for obj in glob.glob(src_path + "/*"):
        print obj

        if os.path.isdir(obj):
            # the directory name is user_id
            driver_name = os.path.basename(obj)
            for f in glob.glob(obj + "/*.csv"):
                # the file name is trip_name
                # trip_name = os.path.basename(os.path.splitext(f)[0])

                # new_file_name = trip_name + ".new"

                try:
                    old_file = open(f, 'r')

                    new_file_path = os.path.dirname(tar_path + "/")
                    if not os.path.exists(new_file_path):
                        os.makedirs(new_file_path)

                    if record == "":
                        file_count += 1
                        new_file = open(new_file_path + "/" + str(file_count) + ".new", 'w')

                    before_pt = 0
                    before_velocity = 0.0
                    seq_num = 0

                    for line in old_file.readlines()[1:]:
                        seq_num += 1
                        current_pt = line.strip("\n")

                        if 0 == before_pt:
                            before_pt = current_pt

                        distance, velocity = basic_features(before_pt.split(","), current_pt.split(","))
                        acceleration = velocity - before_velocity

                        #가속 : 1, 감속:2, 정속: 3
                        accel_property = 0
                        acc_num = int(acceleration)
                        if acc_num > 0:
                            accel_property = 1
                        elif acc_num < 0:
                            accel_property = 2
                        elif acc_num == 0:
                            accel_property = 3

                        # 운전자ID, 주행ID, 시퀀스, X, Y, 이동거리, 속도, 가속도, 가속도 구분
                        record += driver_name + "," + trip_name + "," + str(seq_num) + "," + line.strip("\n") + "," + \
                                   str(distance) + "," + str(velocity) + "," + str(acceleration) + "," + \
                                   str(accel_property) + "\n"


                        # # 파일 생성
                        # if len(record) >= FILE_SIZE:
                        #     new_file.write(record)
                        #     logging.info(record.strip('\n'))
                        #     record = ""

                        before_pt = current_pt
                        before_velocity = velocity

                    old_file.close()
                    new_file.close()
                except OSError as e:
                    print e
                    # print "I/O error({0}): {1}".format(e.errno, e.strerror)


if __name__ == "__main__":
    #sys.argv = [r"D:\Workspace\Data\Kaggle\Driver_Telematics_Analysis\sample-drivers", r"D:\Workspace\Data\Kaggle\Driver_Telematics_Analysis\result-drivers"]
    dir_path = sys.argv
    parse_data(dir_path[1], dir_path[2])

