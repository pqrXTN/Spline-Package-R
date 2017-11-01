# NEWS

## splineABS 0.1.1

### Improvements
* Add parameter `splineFormula` in function `adap.ssanova` and `compare.mse`. We can use formatted string as formula in spline of high dimension.

* New function `find.bounday`: 
  Add boundary points of independent varibles to initial adaptive sampling set.
  
* Modify function `adap.ssanova` and `compare.mse` for the use of `find.bounday`.


## splineABS 0.1.2

### Improvements

* New function `gen.subset` and `adap.divide.ssanova`: 
  Do ABSspline in subsets and average the fitting value.
  
* Modify function `adap.sampling`; add new features in sampling with subsets.


## splineABS 0.1.3

### Improvements

* Modify function `gen.subset`. Now, it can return a list of indices in each subsets without input of `sample.index`.

## splineABS 0.1.4

### Improvements

* Modify function `gen.subset`, `adap.divide.ssanova`, `adap.ssanova`.
  Now, every subset in ABSpline shares the same basis. Copy basis into each subsets to to garentee this. The modified functions are adapted for this change.