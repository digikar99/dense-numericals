import numpy as np 
import pickle as pkl
import sys
import os

dirname = sys.argv[1] if len(sys.argv)>1 else "./"

def fetch_data_from_csv_and_dump():
	train=np.loadtxt(os.path.expanduser(dirname + "/mnist_train.csv"),
					 skiprows=1,
					 delimiter=",")
	test=np.loadtxt(os.path.expanduser(dirname + "/mnist_test.csv"),
				 skiprows=1,
				 delimiter=",")
	print(train.shape)
	print(test.shape)
	np.save(open("train_x.npy","wb"), train[:, 1:])
	np.save(open("train_y.npy","wb"), train[:, 0])
	np.save(open("test_x.npy", "wb"), test[:, 1:])
	np.save(open("test_y.npy","wb"), test[:, 0])

fetch_data_from_csv_and_dump()
