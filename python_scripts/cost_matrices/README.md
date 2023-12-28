# Cost matrices for SIAM

The csv files contain matrices that represent different distance cost metrics between municipalities (the indexes follow the order of MunicipalitiesCentroids.csv). Given $d$ the distance between two municipalities, the cost function in each file are:

- cost_matrix_0.csv: $\quad\frac{1}{\sqrt{d}}$
- cost_matrix_1.csv: $\quad\frac{1}{d}$
- cost_matrix_2.csv: $\quad\frac{1}{d^{1.5}}$
- cost_matrix_3.csv: $\quad\frac{1}{d^2}$
- cost_matrix_4.csv: $\quad e^{-d}$