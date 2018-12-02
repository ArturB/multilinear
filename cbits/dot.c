
// simple dot product
double dot(double* v1, double* v2, int s) {
    double res = 0; 
    for(int i = 0; i < s; i++) {
        res = res + v1[i] * v2[i];
    }
    return res;
}

// generic zip function
void zip(double (*f)(double, double), double* v1, double* v2, int s, double* res) {
    for(int i = 0; i < s; ++i) {
        res[i] = (*f)(v1[i], v2[i]);
    }
}
