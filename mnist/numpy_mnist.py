import numpy as np 
import pickle as pkl
def fetch_data_from_csv_and_dump_pkl():
	x=np.loadtxt("mnist_train.csv",skiprows=1,delimiter=",")
	y=np.loadtxt("mnist_test.csv",skiprows=1,delimiter=",")
	print(x.shape)
	print(y.shape)
	pkl.dump(x,open("train","wb"))
	pkl.dump(y,open("test","wb"))
	pkl.dump(x[:100],open("train_small","wb"))
	pkl.dump(y[:100],open("test_small","wb"))

def fetch_data_from_pkl(file):
	x=pkl.load(open(file,"rb"))
	return x[:,1:],x[:,0].reshape((-1))

def xavier_init(fan_in,fan_out,shape):
	x=np.sqrt(6)/np.sqrt(fan_out+fan_in)
	return np.random.uniform(-x,x,size=shape)

class Tanh(object):
	def __init__(self):
		self.func=np.tanh
		self.weights={}
		self.requires_grad=False

	def __call__(self,batch):
		self.context=self.func(batch)
		return self.context

	def backward(self,grd_wrt_out):
		#------ out = tanh ( input ), to cal d o/d input= d o/d out * d out/d input
		return grd_wrt_out* (1-self.context*self.context)

class Softmax(object):
	def __init__(self):
		self.weights={}
		self.requires_grad=False

	def __call__(self,batch):
		self.context=np.exp(batch)
		self.output= self.context/np.sum(self.context,1,keepdims=True)
		return self.output

	def backward(self,grd_wrt_out):
		#------ out = tanh ( input ), to cal d o/d input= d o/d out * d out/d input
		num_classes=self.output.shape[1]
		batch_size=self.output.shape[0]
		x=np.tile(np.expand_dims(self.output,1),(1,num_classes,1))
		y=np.tile(np.expand_dims(self.output,2),(1,1,num_classes))
		I=np.tile(np.expand_dims(np.ones((num_classes,num_classes)),0),(batch_size,1,1))
		return np.squeeze((x*(I-y))@np.expand_dims(grd_wrt_out,2),-1)

class Linear(object):
	def __init__(self,in_dim,out_dim,bias=True):
		self.A=xavier_init(in_dim,out_dim,shape=(in_dim,out_dim))
		self.b=xavier_init(in_dim,out_dim,shape=(out_dim))
		self.dA=None
		self.db=None
		self.requires_grad=True
		# self.weights={"A":[self.A,self.dA],"b":[self.b,self.db]}

	def __call__(self,batch):
		self.input=batch
		return batch@self.A+self.b

	def backward(self,dout):
		#------ out =  input * A + b, to cal dl/dinput= d o/d out * d out/d input
		self.dA=self.input.T@dout  #dout=(b x m)  A=(n x m) input=(b x n)
		self.db=np.sum(dout,0) #dout=(b x m)  A=(n x m)
		# print(dout.shape,self.A.shape)
		return dout@(self.A.T) #dout=(b x m)  A=(n x m) 

	def grad_update(self,lr):
		self.A-=lr*self.dA
		self.b-=lr*self.db

class BCELoss(object):
	def __init__(self):
		self.requires_grad=False

	def __call__(self,batch,y):
		self.batch=batch
		self.y=y.astype(int)
		return np.mean(-np.log(batch[self.y]))
	def backward(self):
		grad=np.zeros_like(self.batch)
		x=self.batch.shape[0]
		grad[self.y]=-1/(x*self.batch[self.y])
		return grad


def forward(module_list,batch):
	out=batch
	for i in range(len(module_list)):
		out=module_list[i](out)
	return out

def backprop_and_update(module_list,loss_object,lr):
	dout=loss_object.backward()
	for i in range(len(module_list)-1,-1,-1):
		# print("dout shape",dout.shape)
		# print(module_list[i])
		dout=module_list[i].backward(dout)
		if module_list[i].requires_grad:
			module_list[i].grad_update(lr)

		
def evaluate(x,module_list,y):
	predict=[]


	for i in range(len(x)):
		batch_x=x[i]
		# batch_y=dataloader_y_train[i]
		out=forward(module_list,batch_x)
		predict.append(np.argmax(out,-1))
	y=np.concatenate(y).astype(int)
	predict=np.concatenate(predict).astype(int)
	correct=np.sum((predict==y))
	return correct/y.shape[0]

inp_feat=784
out_clases=10
# module_list=[Linear(inp_feat,128),Tanh(),Linear(128,9),Softmax()] 
module_list=[Linear(inp_feat,9),Softmax()] 
loss_object=BCELoss()

x,y=fetch_data_from_pkl("train")
x=x/255-0.5
batch_size=1000
dataloader_x_eval=np.array_split(x,batch_size)
dataloader_y_eval=np.array_split(y,batch_size)

acc=evaluate(dataloader_x_eval,module_list,dataloader_y_eval)
print("acc -1",acc)

for epoch in range(100):
	rdm_idx=np.random.permutation(x.shape[0])
	new_x=x[rdm_idx]
	new_y=y[rdm_idx]
	dataloader_x_train=np.array_split(new_x,batch_size)
	dataloader_y_train=np.array_split(new_y,batch_size)
	for i in range(len(dataloader_x_train)):
		batch_x=dataloader_x_train[i]
		batch_y=dataloader_y_train[i]
		output=forward(module_list,batch_x)
		loss=loss_object(output,batch_y)
		backprop_and_update(module_list,loss_object,1e-3)
	# print(loss)
	acc=evaluate(dataloader_x_eval,module_list,dataloader_y_eval)
	print("acc",acc)






