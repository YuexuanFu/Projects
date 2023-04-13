import numpy as np
from tensorflow.keras.models import load_model
from tensorflow.keras.applications.vgg16 import preprocess_input
from keras.preprocessing import image

import os
import glob
import sys


def get_files(path):
    if os.path.isdir(path):
        files = glob.glob(os.path.join(path, '*'))
    elif path.find('*') > 0:
        files = glob.glob(path)
    else:
        files = [path]

    files = [f for f in files if f.endswith('JPG') or f.endswith('jpeg') or f.endswith('jpg') or f.endswith('PNG') or f.endswith('png')]

    if not len(files):
        sys.exit('No images found by the given path!')

    return files


Fully_Cover_TEST_PATH = '/Users/yuexuanfu/Desktop/Deep Learning/Facemask_detection/model/dataset/testing/fully_covered'
Not_TEST_PATH = '/Users/yuexuanfu/Desktop/Deep Learning/Facemask_detection/model/dataset/testing/not_covered'
Partially_TEST_PATH = '/Users/yuexuanfu/Desktop/Deep Learning/Facemask_detection/model/dataset/testing/partially_covered'
Optional_TEST_PATH = '/Users/yuexuanfu/Desktop/Deep Learning/Facemask_detection/model/optional_test'
if __name__ == '__main__':
    files = get_files(Optional_TEST_PATH)
    cls_list = ['fully_covered', 'not_covered', 'partially_covered']

    # load the trained modelfully_
    net = load_model('./model/model_name.h5')

    # loop through all files and make predictions
    totalData, Fully_Covered_Num, Not_Covered_Num, Patially_Covered_Num = 0, 0, 0, 0
    for f in files:
        img = image.load_img(f, target_size=(64, 64))
        if img is None:
            continue
        totalData = totalData + 1
        x = image.img_to_array(img)
        x = preprocess_input(x)
        x = np.expand_dims(x, axis=0)
        pred = net.predict(x)[0]
        top_inds = pred.argsort()[::-1][:5]
        if cls_list[top_inds[0]] == 'partially_covered':
            Patially_Covered_Num = Patially_Covered_Num + 1
        elif cls_list[top_inds[0]] == 'fully_covered':
            Fully_Covered_Num = Fully_Covered_Num + 1
            # print(f)
        elif cls_list[top_inds[0]] == 'not_covered':
            Not_Covered_Num = Not_Covered_Num + 1
        print(f)
        for i in top_inds:
            print('    {:.3f}  {}'.format(pred[i], cls_list[i]))
    print('Total data: {}\npartially_covered num: {}\nfully_covered num: {}\nnot_covered num: {}\n'
          .format(totalData, Patially_Covered_Num, Fully_Covered_Num, Not_Covered_Num))
