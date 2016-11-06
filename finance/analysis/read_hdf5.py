from __future__ import print_function
import numpy as np
import h5py

with h5py.File('snp500.h5') as hf:
	print('List of arrays: \n', hf.keys())

	data=hf.get('energy')
	np_data = np.array(data)
	print('Shape of the array dataset: \n', np_data.shape)
	
