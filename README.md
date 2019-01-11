# FitResponseStyles

FitResponseStyles is a repository containing modeling code using the R Package to fit Divide-by-Total modeling approaches accounting for response styles.
Response styles, such as extreme, acquiescence, or mid response styles, are a potential source of bias when inferences are drawn from rating scale measurement. 

## Usage
To use the modeling code, download and unzip the folder and open the R Project file. 
In the data folder, you find a simulated dataset with the structure of the German Big Five questionnaire (NEO-FFI; 5 dimensions, 12 items per dimensions, some reversed coded items),
and a file containing the item numbers of reversed coded items in the generated data. 
In the analysis folder, you find a main R file "FitModels" that fits the single modeling files in the folder "fit_models". 
Please adapt the modeling code to match your own data. 

The models presented here are:

## Based on the literature
- Partial Credit Model (ignoring response styles)
- Random Threshold Model (Wang et al., 2006)
- Generalized Random Threshold Model (adapted from Wang & Wu, 2011)
- Multidimensional NRM with estimated scoring weights for response styles (Bolt & Johnson, 2009)
- Multidimensional PCM with category preferences parameters (Bolt et al., 2014)
- Multidimensional PCM with fixed scoring weights for response styles (Bolt & Newton, 2011; Wetzel & Carstensen, 2017)
- Generalized Multidimensional PCM with fixed scoring weights and estimated discrimination parameters (Falk & Cai, 2016)

## Modeling extensions:
- A model lifting equality constraints on scoring weights of ARS
- A model putting equality restrictions on discrimination parameters based on dummy-coded item attributes
- A model putting equality restrictions on discrimination parameteters randomly


# References:
Bolt, D. M., & Johnson, T. R. (2009). Addressing score bias and differential item functioning due to individual differences in response style. Applied Psychological Measurement, 33, 335–352. https://doi.org/10.1177/0146621608329891

Bolt, D. M., Lu, Y., & Kim, J.-S. (2014). Measurement and control of response styles using anchoring vignettes: A model-based approach. Psychological Methods, 19, 528–541. https://doi.org/10.1037/met0000016

Bolt, D. M., & Newton, J. R. (2011). Multiscale measurement of extreme response style. Educational and Psychological Measurement, 71(5), 814–833. https://doi.org/10.1177/0013164410388411

Falk, C. F., & Cai, L. (2016). A flexible full-information approach to the modeling of response styles. Psychological Methods, 21, 328–347. https://doi.org/10.1037/met0000059

Masters, G. N. (1982). A Rasch model for partial credit scoring. Psychometrika, 47, 149–174. https://doi.org/10.1007/BF02296272

Wang, W.-C., Wilson, M., & Shih, C.-L. (2006). Modeling randomness in judging rating scales with a random-effects rating scale model. Journal of Educational Measurement, 43, 335–353. https://doi.org/10.1111/j.1745-3984.2006.00020.x

Wang, W.-C., & Wu, S.-L. (2011). The random-effect generalized rating scale model. Journal of Educational Measurement, 48, 441–456. https://doi.org/10.1111/j.1745-3984.2011.00154.x

Wetzel, E., & Carstensen, C. H. (2017). Multidimensional modeling of traits and response styles. European Journal of Psychological Assessment, 33, 352–364. https://doi.org/10.1027/1015-5759/a000291