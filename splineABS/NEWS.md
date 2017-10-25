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


