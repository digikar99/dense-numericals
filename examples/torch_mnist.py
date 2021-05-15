import torch as t
import numpy as np
import os
import time

train_x, train_y, test_x, test_y = [None]*4

def one_hot_encode(vector, num_classes):
	zeros = np.zeros((vector.shape[0], num_classes))
	for idx in range(vector.shape[0]):
		col = vector[idx]
		zeros[idx, int(col)] = 1
	return zeros

def load_data():
	global train_x, train_y, test_x, test_y
	x  = np.load(os.path.expanduser("~/ram-disk/train_x.npy"))
	ir = np.concatenate(
		(np.ones((x.shape[0], 1)), x),
		axis = 1
	)
	ir /= 255
	train_x = t.tensor(ir - (ir.max() + ir.min())/2)
	train_y = t.tensor(
		one_hot_encode(
			np.load(os.path.expanduser("~/ram-disk/train_y.npy")),
			10
		)
	)
	x  = np.load(os.path.expanduser("~/ram-disk/test_x.npy"))
	ir = np.concatenate(
		(np.ones((x.shape[0], 1)), x),
		axis=1
	)
	ir /= 255
	test_x = t.tensor(ir - (ir.max() + ir.min())/2).float()
	test_y = t.tensor(
		one_hot_encode(
			np.load(os.path.expanduser("~/ram-disk/test_y.npy")),
			10
		)
	)

def softmax(x):
	assert len(x.shape)==2
	t.exp(x, out=x)
	t.divide(
		x,
		t.reshape(t.sum(x, axis=1), (x.shape[0], 1)),
		out=x)
	return x

def cross_entropy(y_predicted, y_true):
	y_predicted = y_true * t.subtract(t.tensor(0), (t.log(y_predicted))) \
		+ y_predicted * (1-y_true)
	return y_predicted

LEARNING_RATE = 0.1

def predict(weights, x, x_idx=None):
	x = (x if x_idx is None else x[x_idx:1+x_idx])
	return softmax(t.matmul(x, weights))

def accuracy(weights, x, y):
	predictions = predict(weights, x)
	num_total   = y.shape[0]
	num_correct = t.sum((t.argmax(predictions, axis=1) == t.argmax(y, axis=1)))
	return num_correct/num_total

def fit(train_x, train_y, num_iterations,
		loss_interval = None,
		batch_size = 4,
		weights = None):
	loss_interval = (num_iterations//10 if loss_interval is None else loss_interval)
	num_y_classes = train_y.shape[1]
	num_images    = train_x.shape[0]
	weights = (t.rand((train_x.shape[1], num_y_classes), dtype=t.float64)
			   if weights is None else weights)
	grad    = t.zeros_like(weights)
	y_predicted      = t.zeros((batch_size, num_y_classes), dtype=t.float64)
	softmax_y_holder = t.zeros_like(y_predicted)
	cross_entropy_y_holder = t.zeros_like(y_predicted)
	x_holder = t.zeros((batch_size, train_x.shape[1]), dtype=t.float64)

	# try:

	start = time.time()
	
	for iter in range(num_iterations):

		loss = 0

		for img_idx in range(0, num_images, batch_size):

			t.matmul(
				train_x[img_idx:img_idx+batch_size],
				weights,
				out = y_predicted
			)
			softmax_y_holder[:] = y_predicted[:]
			softmax(softmax_y_holder)

			y_true = train_y[img_idx:img_idx+batch_size]
			cross_entropy_y_holder[:] = softmax_y_holder[:]
			loss += t.sum(cross_entropy(cross_entropy_y_holder, y_true))

			t.subtract(y_true, softmax_y_holder, out=softmax_y_holder)
			t.matmul(
				train_x[img_idx:img_idx+batch_size].T,
				softmax_y_holder,
				out = grad
			)
			weights += LEARNING_RATE/num_images * grad

		if iter % loss_interval == 0: print(loss)

	end = time.time()
	print("Running", num_iterations, "took", end-start, "seconds!")
	# finally:
	return weights

load_data()
fit(train_x, train_y, 50)
