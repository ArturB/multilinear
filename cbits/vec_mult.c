////////////////////////////////////////////////////////////////////////////////

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <CL/cl.h>
#include <stdbool.h>

////////////////////////////////////////////////////////////////////////////////
#define WA 2048
#define WB 2048
////////////////////////////////////////////////////////////////////////////////

double multAndAddVectors(double* A, 
                          double* B,
                          const int len, cl_context context, cl_command_queue commands, cl_kernel kernel)
{
    unsigned int size_C = len;
   unsigned int mem_size_C = sizeof(double) * size_C;
   double* C = (double*) malloc(mem_size_C); 
  cl_mem d_A;
   cl_mem d_B;
   cl_mem d_C;   

int err;
// Create the input and output arrays in device memory for our calculation
   d_C = clCreateBuffer(context, CL_MEM_READ_WRITE, mem_size_C, NULL, &err);
   d_A = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, sizeof(A)*len, A, &err);
   d_B = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, sizeof(B)*len, B, &err);

   if (!d_A || !d_B || !d_C)
   {
    //    printf("Error: Failed to allocate device memory!\n");
       exit(1);
   }    

   
	//Launch OpenCL kernel
   size_t localWorkSize[2], globalWorkSize[2];

   localWorkSize[0] = 1;
   localWorkSize[1] = 1;
   globalWorkSize[0] = len;
   globalWorkSize[1] = len;

   err = clEnqueueNDRangeKernel(commands, kernel, 2, NULL, globalWorkSize, localWorkSize, 0, NULL, NULL);
    
   if (err != CL_SUCCESS)
   {
    //    printf("Error: Failed to execute kernel! %d\n", err);
       exit(1);
   }
   //Retrieve result from device
   err = clEnqueueReadBuffer(commands, d_C, CL_TRUE, 0, mem_size_C, C, 0, NULL, NULL);

   if (err != CL_SUCCESS)
   {
    //    printf("Error: Failed to read output array! %d\n", err);
       exit(1);
   }
    double result = 0;
    for(int i = 0; i<len; ++i )
        result += C[i];

   free(C);
   clReleaseMemObject(d_A);
   clReleaseMemObject(d_C);
   clReleaseMemObject(d_B);

    return result;
}
long LoadOpenCLKernel(char const* path, char **buf);

int final(cl_context context, cl_command_queue commands, cl_program program, cl_kernel kernel){

   clReleaseProgram(program);
   clReleaseKernel(kernel);
   clReleaseCommandQueue(commands);
   clReleaseContext(context);
}
/*LOAD ME TO HASKELL */
int setup()
{

   int err;                            // error code returned from api calls

   cl_device_id device_id;             // compute device id 
   cl_context context;                 // compute context
   cl_command_queue commands;          // compute command queue
   cl_program program;                 // compute program
   cl_kernel kernel;                   // compute kernel

    // OpenCL device memory for matrices
//    cl_mem d_A;
//    cl_mem d_B;
//    cl_mem d_C;

      //Allocate host memory for the result vector C


//    printf("Initializing OpenCL device...\n"); 
   cl_uint dev_cnt = 0;
   clGetPlatformIDs(0, 0, &dev_cnt);
	
   cl_platform_id platform_ids[100];
   clGetPlatformIDs(dev_cnt, platform_ids, NULL);
	
   // Connect to a compute device
   int gpu = 1;
   err = clGetDeviceIDs(platform_ids[0], gpu ? CL_DEVICE_TYPE_GPU : CL_DEVICE_TYPE_CPU, 1, &device_id, NULL);
   if (err != CL_SUCCESS)
   {
    //    printf("Error: Failed to create a device group!\n");
       return EXIT_FAILURE;
   }
  
   // Create a compute context 
   context = clCreateContext(0, 1, &device_id, NULL, NULL, &err);
   if (!context)
   {
    //    printf("Error: Failed to create a compute context!\n");
       return EXIT_FAILURE;
   }

   // Create a command commands
   commands = clCreateCommandQueue(context, device_id, 0, &err);
   if (!commands)
   {
    //    printf("Error: Failed to create a command commands!\n");
       return EXIT_FAILURE;
   }

   // Create the compute program from the source file
   char *KernelSource;
   long lFileSize;

   lFileSize = LoadOpenCLKernel("vec_mult.cl", &KernelSource);
   if( lFileSize < 0L ) {
    //    perror("File read failed");
       return 1;
   }

   program = clCreateProgramWithSource(context, 1, (const char **) & KernelSource, NULL, &err);
   if (!program)
   {
    //    printf("Error: Failed to create compute program!\n");
       return EXIT_FAILURE;
   }

   // Build the program executable
   err = clBuildProgram(program, 0, NULL, NULL, NULL, NULL);
   if (err != CL_SUCCESS)
   {
       //size_t len;
       //char buffer[2048];
    //    printf("Error: Failed to build program executable!\n");
      // clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG, sizeof(buffer), buffer, &len);
    //    printf("%s\n", buffer);
       exit(1);
   }

   // Create the compute kernel in the program we wish to run
   //
   kernel = clCreateKernel(program, "matrixMul", &err);
   if (!kernel || err != CL_SUCCESS)
   {
    //    printf("Error: Failed to create compute kernel!\n");
       exit(1);
   }

//    // Create the input and output arrays in device memory for our calculation
//    d_C = clCreateBuffer(context, CL_MEM_READ_WRITE, mem_size_C, NULL, &err);
//    d_A = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, sizeof(A)*len, A, &err);
//    d_B = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, sizeof(B)*len, B, &err);

//    if (!d_A || !d_B || !d_C)
//    {
//     //    printf("Error: Failed to allocate device memory!\n");
//        exit(1);
//    }    

//    //Launch OpenCL kernel
//    size_t localWorkSize[2], globalWorkSize[2];
 
//    err = clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&d_C);
//    err |= clSetKernelArg(kernel, 1, sizeof(cl_mem), (void *)&d_A);
//    err |= clSetKernelArg(kernel, 2, sizeof(cl_mem), (void *)&d_B);
//    err |= clSetKernelArg(kernel, 3, sizeof(int), (void *)&len);
// //    err |= clSetKernelArg(kernel, 4, sizeof(int), (void *)&wC);

//    if (err != CL_SUCCESS)
//    {
//     //    printf("Error: Failed to set kernel arguments! %d\n", err);
//        exit(1);
//    }
 
//    localWorkSize[0] = 1;
//    localWorkSize[1] = 1;
//    globalWorkSize[0] = len;
//    globalWorkSize[1] = len;

//    err = clEnqueueNDRangeKernel(commands, kernel, 2, NULL, globalWorkSize, localWorkSize, 0, NULL, NULL);
    
//    if (err != CL_SUCCESS)
//    {
//     //    printf("Error: Failed to execute kernel! %d\n", err);
//        exit(1);
//    }
//    //Retrieve result from device
//    err = clEnqueueReadBuffer(commands, d_C, CL_TRUE, 0, mem_size_C, C, 0, NULL, NULL);

//    if (err != CL_SUCCESS)
//    {
//     //    printf("Error: Failed to read output array! %d\n", err);
//        exit(1);
//    }
//     double result = 0;
//     for(int i = 0; i<len; ++i )
//         result += C[i];
//    printf("%f\n", result[0]);
//    printf("\n\nMatrix C (Results)\n");
//    printf("\n");

//    printf("Matrix multiplication completed...\n"); 

   //Shutdown and cleanup
 



    
    return 0;
 }
// Allocates a matrix with random float entries.
void randomMemInit(double* data, int size)
{
   int i;

   for (i = 0; i < size; ++i)
   {
    // data[i] = rand() / (double)RAND_MAX;
    data[i] = 1;
   }
}

long LoadOpenCLKernel(char const* path, char **buf)
{
    FILE  *fp;
    size_t fsz;
    long   off_end;
    int    rc;

    /* Open the file */
    fp = fopen(path, "r");
    if( NULL == fp ) {
        return -1L;
    }

    /* Seek to the end of the file */
    rc = fseek(fp, 0L, SEEK_END);
    if( 0 != rc ) {
        return -1L;
    }

    /* Byte offset to the end of the file (size) */
    if( 0 > (off_end = ftell(fp)) ) {
        return -1L;
    }
    fsz = (size_t)off_end;

    /* Allocate a buffer to hold the whole file */
    *buf = (char *) malloc( fsz+1);
    if( NULL == *buf ) {
        return -1L;
    }

    /* Rewind file pointer to start of file */
    rewind(fp);

    /* Slurp file into buffer */
    if( fsz != fread(*buf, 1, fsz, fp) ) {
        free(*buf);
        return -1L;
    }

    /* Close the file */
    if( EOF == fclose(fp) ) {
        free(*buf);
        return -1L;
    }


    /* Make sure the buffer is NUL-terminated, just in case */
    (*buf)[fsz] = '\0';

    /* Return the file size */
    return (long)fsz;
}



// int main(int argc, char** argv)
// {
 
//    // set seed for rand()
//    srand(2014);
 
//    //Allocate host memory for matrices A and B
//    unsigned int size_A = WA;
//    unsigned int mem_size_A = sizeof(double) * size_A;
//    double* h_A = (double*) malloc(mem_size_A);
 
//    unsigned int size_B = WB;
//    unsigned int mem_size_B = sizeof(double) * size_B;
//    double* h_B = (double*) malloc(mem_size_B);

//    //Initialize host memory
//    randomMemInit(h_A, size_A);
//    randomMemInit(h_B, size_B);
 
//    double result = multAndAddVectors(h_A, h_B, size_A);
//     printf("\nRESULT=%f\n", result);

//    //Shutdown and cleanup
//    free(h_A);
//    free(h_B);
// }