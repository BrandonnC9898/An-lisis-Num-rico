def funcionD(n):
	c = 0
	print("Binario = ",end="")
	while(n > 0):
		d = n%2
		n = n//2
		print(d, end="")
		c = c+1
	return c
