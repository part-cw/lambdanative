double  *newArray(int sz){ 
	double *ptr = malloc(sizeof(double)*sz);
	if (ptr != 0){
		return (void *)ptr;
	} else {
		return 0;
	}
}

int arrayInsert(double *arr, int index, double val){ 
	//puts("entered array insert");
	if (arr != 0){ 
		arr[index] = val;
		//printf("%p inserted %f at %d\n",arr,arr[index],index);
	};
	//puts("exiting array insert");
	return 0;
}

int genannSave(genann *ann,const char *fn){
	FILE *f = fopen(fn,"w");
	if (f){
		genann_write(ann,f);
		fclose(f);
	} else {
		puts("Error opening file!");
	}
	return 0;
}

genann  *genannLoad(const char *fn){
	FILE *f = fopen(fn,"r");
	if (f){ 
		genann *ann = genann_read(f);
		fclose(f);
		return (void *)ann;
	} 
	return 0;
}

genann *genannCopy(genann *ann){
	genann *g = genann_copy(ann); 
	if (g){ 
		return (void *)g; 
	} 
	return 0;
}

int genannRandomize(genann *ann){
	genann_randomize(ann);
	return 0;
}

int genannFree(genann *ann){
	genann_free(ann);
	return 0;
}

double *genannRun(genann *ann,double const *inputs){ 
	const double *r = genann_run(ann,inputs);
	return (double *)r; 
}

int arrayFree(double *arr){
	free(arr);
	return 0;
}

double peek(double *arr, int i){ 
	if (i < sizeof(arr)){ 
		return arr[i];
	} return 0;
}

int showArray(double *arr, int len){
	//printf("The size of the array is %d\n",len);
	for (int i = 0; i < len; ++i){
		//printf("%p %d %f %d\n",arr,i,arr[i],len);
	}
	return 0;
}

int genannTrain(genann *ann, double *inputs, int iLen, double *outputs, int oLen, double rate){
	//printf("Got %d inputs and %d outputs at rate %f\n",iLen,oLen,rate);
	//puts("Inputs");
	for (int i = 0; i < iLen; ++i){
		//printf("%d %f\n",i,inputs[i]);
	}
	//puts("Outputs");
	for (int i = 0; i < oLen; ++i){
		//printf("%d %f\n",i,outputs[i]);
	}
	genann_train(ann,inputs,outputs,rate);
	return 0;
}

int getNInputs(genann *ann){
	return ann->inputs;
}

int getNHiddenLayers(genann *ann){
	return ann->hidden_layers;
}

int getNHidden(genann *ann){
	return ann->hidden;
}

int getNOutputs(genann *ann){
	return ann->outputs;
}

int getNTotalWeights(genann *ann){
	return ann->total_weights;
}

int getNTotalNeurons(genann *ann){
	return ann->total_neurons;
}

double *getWeights(genann *ann){
	return ann->weight;
}

double *getOutput(genann *ann){
	return ann->output;
}

double *getDelta(genann *ann){
	return ann->delta;
}

int randomSeed(){
	srand(time(0));
	return 0;
}
