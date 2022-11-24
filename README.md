# Programming Language Design - CS 476 - Final Project
## Language for Sparse Matrix Calculus


Contributors: Sofia Lucca, Andrea Carotti

The language designed is a basic linear algebra language. The main difference between our language (like [Simit](http://simit-lang.org/)) and other linear algebra languages, such as Matlab and Julia, is in the use of data structure to represent the data it handles, especially sparse systems.



## Main Features of the Language
- Simple imperative language features already implemented
- Arrays and Matrix declaration (COO format)
- Ability to parse a matrix declared as a long array of given M rows and N columns
- Sum and Subtraction between sparse matrices in COO format
- Multiplication between a matrix A (COO format) and a generic array x
- Multiplication between a matrix A (COO format) and a matrix B (COO format)
- Transpose of matrix A (COO format)
- Basic operations to get useful matrix information:
    - Trace of the matrix A
    - Sparsity of the matrix A
    - Density of the matrix A

## Introduction
Sparse Computation is an operation used in many application domains. Our language tries to address the problem of reading from input a matrix as a long vector (given for example from an input file) parse it, and then perform basic linear algebra operations.

## Types
The types implemented in our lanugage, that extends a simple basic imperative laguage are Vector and the specific types to handle different formats of sparse matrices. In our project we implemented the COO format (Coordinate List) but the same paradigm can be adopted to handle operations with other formats.

## Parse operation
Given a generic list of int with specific support functions we were able to parse the given list into a COO matrix.
```sh
CreateCOO("A", Num 4, Num 4, Vector ([1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]))
```
This operation lets you store inside a COO matrix the original int list provided.

<img src="img/coo_format.png" width="600" height="200" />


## Sum operation C = A+B
The main idea is going through the coordinates of row and colum of the two parsed matrices, whenever they match the sum between them is performed. In case there is no match the value for that coordinate is still is added to the result. 
The main difficulty for this approach was passing the declaration through the process and extracting the rows and columns indeces from the data object.
```sh
MatSum("C", Var "A", Var "B")
```
Before adding to the resut a check is performed for the results which are 0 that don't have to be added to the result.

## Sub operation C = A-B
The approach for subtraction is analogous to the one for sum.
```sh
MatSub("C", Var "A", Var "B")
```

## Multiplication operation y = A*x
the original C algorithm used for this task suits perfectly OCaml ways of handling lists, since to perform the multiplication you need to scan the rows, columns and data arrays (used to store the matrix) only once.
```sh
MatMul("y", Var "A", Var "x")
```

<img src="img/matrix_vector_mul.png" width="1000" height="200" />


## Multiplication operation C = A*B
The implementation of this operation was much harder instead. Intuitively the first way to solve the problem would be to treat the B matrix as a group of columns arrays and try to perform the same operation as before. The objective though is to keep matrices in sparse format, so the problem was not solved with this method.

```sh
MatMul("C", Var "A", Var "B")
```

<img src="img/matrix_matrix_mul.png" width="900" height="350" />



The algorithm proposed first transpose the B matrix, and then scan the support data structures as a "nested" for loop.

Here is the link containing the main idea of the algorithm: https://www.youtube.com/watch?v=x70zNUIHR0k&t=306s&ab_channel=ComputerEngineeringmadeEasy

The last challenging part then is to compress the representation obtained, this is much harder and to achieve this, we first transformed the triplets of the data structures from lists to triples, then we sorted them, then we retransoformed to lists (we couldn't just sort the lists because the i-th value of the list is bounded for the triplets) and finally we applied the compression algorithm to get the final lists.


## Future Work

Future work include extending this language to other formats, like CSR, that is the easier one to implement, then other formats as well.


# List of language features implemented

NEW EXPRESSIONS INTRODUCED:
- Trace
- Density
- Sparsity

NEW COMMAND INTRODUCED:
- Creation of the matrix
- Sum between two given matrices
- Subtraction between two given matrices
- Multiplication between:
	- a matrix A and a generic vector x
	- a matrix A and another matrix B

NEW TYPES INTRODUCED:
- Vector: implemented through an list of int
- COO: implemented through three lists (rows, cols, and data) and two int inside a record
	- rows holds the row coordinate of the non-zero element
	- cols holds the column coordinate of the non-zero element
	- data holds the value of the non-zero element stored inside the original matrix at the corresponding coordinate given by rows and cols
	- then two int are used to store the number of rows and the number of columns of the original matrix
- Float, in order to get sparsity and density of the matrices

USEFUL SUPPORT FUNCTIONS:
- get_col and get_row: are used to extract the row and the column of the new COO matrix given the original array of int
- get_data_array, get_cols_array and get_rows_array: are used to extract the rows, cols and data vectors of the parsed COO matrix (effectively the data structures used to store the matrix in memory)
- parse_coo: is used to correctly fill the record coodecl

- sum_op_helper and sum_op are the two functions used to perform the sum between two COO matrices. sum_op_helper is recursive while sum_op calls sum_op_helper

- sub_op_helper and sub_op are the two functions used to perform the sum between two COO matrices. sub_op_helper is recursive while sub_op calls sub_op_helper

- get_val_from_idx: is used to extract from a list the given index (uses the nth_opt library function)

- replace: is used to "substitute" (list in OCaml are immutable) in a list the element of in the chosen postion with the chosen element

- coo_Ax_mul_helper and coo_Ax_mul: is the function used to perform the multiplication between matrix A and a vector x (given that the number of columns of the matrix and the length of the vector are the same)

- trace_coo_fun and trace_coo: extract the trace from matrix A in COO format

- get_col_order, re_order_vector and get_coo_transpose are used to to obtain the transpose of matrix A

- coo_partial_matrix_mul_helper and coo_partial_matrix_mul are used to perform the multiplication between two matrices A and B (given that it is possible to do it). But is called partial because the result is not yet a COO matrix for two reasons: first, the elements inside of rows and cols are not in the correct order, second, there are multiple values i and j for which is it true that rows[i] = rows[j] and cols[i] = cols[j] and the content of the elements of data (data[i] and data[j]) for which this happen needs to be added.

- combine_lists is used to obtain the tuples of tuples from the three lists rows cols and data such that:
if rows = 1,2,3 ; cols = 1,2,3; data = 8, 4, 7
then we obtain ((1,1),8) ((2,2),4) ((3,3),7)

- supersort: is just a custom function used to reorder the three row, cols and data array in the final COO format (using the tuples obtained with combine_list).

- compress: get the final multiplication result compressing the data structures obtained with the previous functions

TYPE OF:
- introduced type_of commands for
	- Vector
	- Trace
	- Sparsity
	- Density

TYPE CHECK:
- Introduced typecheck commands for
	- CreateCOO
	- MatSumCOO
	- MatSubCOO
	- MatMulCOO
	
