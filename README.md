# Multidimensional_statistics

This repository contains several notebooks implementing numerical linear algebra algorithms and machine learning models. The numerical methods focus on recursive matrix operations, while the machine learning scripts cover regression and classification techniques.  

## **1. Matrix Multiplication and Recursive Algorithms**  
- Implements matrix multiplication using:  
  - **Traditional algorithm** for matrices of size ≤ \(2^l \times 2^l\)  
  - **Recursive Binet's algorithm** for larger matrices  
- Analyzes execution time and floating-point operations for different matrix sizes \(2^k \times 2^k\), where \(k = 2,3, \dots, 8\) and \(l \in \{3,5,7\}\).  

## **2. Recursive Matrix Inversion**  
- Implements recursive inversion for square matrices of size \(2^k \times 2^k\).  
- Uses the recursive matrix multiplication from the first notebook.  
- Evaluates execution time and floating-point operations for \(k = 2,3, \dots, 6\).  

## **3. Recursive LU Factorization**  
- Implements recursive **LU decomposition** for matrices of size \(2^k \times 2^k\).  
- Uses:  
  - Recursive **matrix multiplication** from Notebook 1  
  - Recursive **matrix inversion** from Notebook 2  
- Analyzes computational complexity for \(k = 1,2, \dots, 8\).  

## **4. Regression and Classification using Decision Trees, Random Forests, and Boosting**  
- **Regression (Life Expectancy Prediction)**:  
  - Decision Trees, Random Forests, and Gradient Boosting (GBM).  
  - Feature selection and importance analysis.  
  - Model evaluation using Mean Squared Error (MSE).  
- **Classification (Titanic Survival Prediction)**:  
  - Decision Trees, Random Forests, and GBM.  
  - Feature selection and model accuracy evaluation.  

## **5. Feature Selection for Regression and Classification**  
- **Best subset selection**, **stepwise selection**, and **Lasso regression**.  
- Applied to **life expectancy prediction** and **Titanic survival classification**.  

## **6. Dimensionality Reduction Analysis**  
- Compares **UMAP, t-SNE, and PaCMAP** on multiple datasets (Reuters, SmallNORB, IVHD, FMNIST).  
- Evaluates using **DR quality metrics, k-NN gain, and trustworthiness scores**.  

Each notebook includes performance analysis, visualizations, and evaluation metrics.
