__kernel void
matrixMul(__global double* result, 
          __global double* A, 
          __global double* B,
          const int wide)
{

   int id = get_global_id(0);

   if(id < wide)
      result[id] = A[id] * B[id];
}