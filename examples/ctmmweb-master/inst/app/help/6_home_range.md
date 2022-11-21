
- Home ranges are calculated for the selected models in model selection table. 
  + You can change the selection in that page if you want to change the targets.
	+ You need to choose the grid for home range calculation first, because certain choice may reasult out of memory error.
	+ They can be calculated in same grid so it's easier to calculate overlaps. However this will need much more memory for spread out individuals.
	+ Alternatively you can calculate them individually to avoid memory error. In this case the overlap feature will be disabled as they are not no longer comparable.
- Check [ctmm::plot.telemetry](https://ctmm-initiative.github.io/ctmm/reference/plot.telemetry.html) for details:
  - `Contour`: `level.UD`. You can input a series of comma separated values like `20, 50, 95` in contours input, note they are percentage values so the real value will be `0.20, 0.50, 0.95`. 
  - `Confidence Envelopes` : `level`.
  - `Location Points` : also draw telemetry locations.
- Read [vignette](https://ctmm-initiative.github.io/ctmm/articles/akde.html) and [?bandwidth](https://ctmm-initiative.github.io/ctmm/reference/bandwidth.html) for detailed discussion of `Optimal Weighting`.
- Home range can be exported to shapefile or raster format. See help inside the export dialog.

### Reference
- [C. H. Fleming, W. F. Fagan, T. Mueller, K. A. Olson, P. Leimgruber, J. M. Calabrese. Rigorous home-range estimation with movement data: A new autocorrelated kernel-density estimator. Ecology, 96:5, 1182-1188 (2015).](https://doi.org/10.1890/14-2010.1)

- [C. H. Fleming, J. M. Calabrese. A new kernel-density estimator for accurate home-range and species-range area estimation. Methods in Ecology and Evolution, 8:5, 571-579 (2016).](https://doi.org/10.1111/2041-210X.12673)

- [C. H. Fleming, D. Sheldon, W. F. Fagan, P. Leimgruber, T. Mueller, D. Nandintsetseg, M. J. Noonan, K. A. Olson, E. Setyawan, A. Sianipar, J. M. Calabrese. Correcting for missing and irregular data in home-range estimation. Ecological Applications, 28:4, 1003-1010 (2018).](https://doi.org/10.1002/eap.1704)
