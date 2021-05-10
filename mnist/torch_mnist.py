import numpy as np 
import pickle as pkl
import torch
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
	return torch.tensor(x[:,1:]).float(),torch.tensor(x[:,0].reshape((-1))).long()
def forward(module_list,x):
	for i in module_list:
		if len(i)==2:
			x=torch.nn.functional.linear(x,i[0],i[1])
		elif len(i)==1:
			x=i[0](x)
	return x
		
def evaluate(x,forward_fn_list,y):
	predict=[]
	for i in range(len(x)):
		batch_x=x[i]
		# batch_y=dataloader_y_train[i]
		out=forward(forward_fn_list,batch_x)
		predict.append(torch.argmax(out,-1))
	y=torch.cat(y).int()
	predict=torch.cat(predict).int()
	correct=torch.sum((predict==y))
	return correct/y.shape[0]

inp_feat=784
out_clases=10
# module_list=[Linear(inp_feat,128),Tanh(),Linear(128,9),Softmax()] 
# module_list=[torch.nn.Parameter(torch.empty(inp_feat,out_clases)),torch.nn.Parameter(torch.empty(1,out_clases))] 
module_list=[[torch.nn.Parameter(torch.empty(out_clases,inp_feat)),torch.nn.Parameter(torch.empty(out_clases))]] 
for x in module_list:
	for i in x:
		if len(i.shape)>1:
			torch.nn.init.xavier_uniform_(i)
		else:
			torch.nn.init.ones_(i)
# activation_fn=lambda x: torch.nn.functional.softmax(x,dim=1)
forward_fn_list=[[module_list[0][0],module_list[0][1]]]
loss_fn=torch.nn.functional.binary_cross_entropy_with_logits
optimizer=torch.optim.SGD([x for i in module_list for x in i],lr=1e-3)


x,y=fetch_data_from_pkl("train")
x=x/255
batch_size=1000
x_test,y_test=fetch_data_from_pkl("test")
x_test=x_test/255
dataloader_x_eval=torch.split(x_test,batch_size)
dataloader_y_eval=torch.split(y_test,batch_size)

acc=evaluate(dataloader_x_eval,module_list,dataloader_y_eval)
print("acc -1",acc)

for epoch in range(100):
	rdm_idx=torch.randperm(x.size(0))
	new_x=x[rdm_idx]
	new_y=y[rdm_idx]
	dataloader_x_train=torch.split(new_x,batch_size)
	dataloader_y_train=torch.split(new_y,batch_size)
	for i in range(len(dataloader_x_train)):
		batch_x=dataloader_x_train[i]
		batch_y=dataloader_y_train[i]
		output=forward(forward_fn_list,batch_x)
		loss=loss_fn(output,torch.nn.functional.one_hot(batch_y).float())
		optimizer.zero_grad()
		loss.backward()
		optimizer.step()
	# print(loss)
	acc=evaluate(dataloader_x_eval,forward_fn_list,dataloader_y_eval)
	print("acc",acc)






