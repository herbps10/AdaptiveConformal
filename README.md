# Adaptive Conformal Inference in R

The `AdaptiveConformal` package implements several Adaptive Conformal
Inference (ACI) algorithms in R.

Conformal Inference is a methodology for constructing prediction intervals from black-box prediction methods.
Classical conformal inference methods, such as Split Conformal Inference, crucially depend on the _exchangeability_ of the observed data (see [Angelopoulos and Bates 2021](https://arxiv.org/abs/2107.07511) for an introduction to conformal inference). Time series data are typically not exchangeable, so many conformal inference methods cannot be applied directly.

Adaptive Conformal Inference (ACI) is a family of algorithms for constructing prediction intervals in an online setting, and are therefore particularly useful for time series data.

The basic idea behind ACI is to adaptively generate prediction intervals that grow or shrink in response to the stream of incoming data.

The following algorithms are included:
- `SCP`: Split Conformal Prediction
- `ACI` / `RollingRC`: Adaptive Conformal Inference ([Gibbs and Candès 2021](https://arxiv.org/abs/2106.00170)), further generalized in the Rolling Risk Control algorithm ([Feldman et al. 2023](https://arxiv.org/abs/2205.09095)).
- `AgACI`: Aggregated Adaptive Conformal Inference ([Zaffran et al. 2022](https://proceedings.mlr.press/v162/zaffran22a.html)).
- `FACI`: Fully Adaptive Conformal Inference ([Gibbs and Candès 2022](https://arxiv.org/abs/2208.08401)).
- `SF-OGD`: Scale-Free Online Gradient Descent ([Orabona and Pál 2018](https://www.sciencedirect.com/science/article/pii/S0304397517308514), [Bhatnagar et al. 2023](https://arxiv.org/abs/2302.07869)).
- `SAOCP`: Strongly Adaptive Online Conformal Prediction ([Bhatnagar et al. 2023](https://arxiv.org/abs/2302.07869)).
