# Adaptive Conformal Inference in R

The `AdaptiveConformal` package implements several Adaptive Conformal
Inference (ACI) algorithms in R.

Conformal Inference is a methodology for constructing prediction intervals from black-box prediction methods.
Classical conformal inference methods, such as Split Conformal Inference, crucially depend on the _exchangeability_ of the observed data. Time series data are typically not exchangeable, so many conformal inference methods cannot be applied directly.

Adaptive Conformal Inference (ACI) is a family of algorithms for constructing prediction intervals in an online setting, and are therefore particularly useful for time series data.

The basic idea behind ACI is to adaptively generate prediction intervals that grow or shrink in response to the stream of incoming data.

The following algorithms have been implemented so far:
- `RollingRC`: Rolling Risk Control. (Note that the original Adaptive Conformal Inference (ACI) algorithm can be recovered as a special case of RollingRC.)
- `AgACI`: Aggregated Adaptive Conformal Inference.
- `FACI`: Fully Adaptive Conformal Inference.
