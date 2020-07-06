#!/bin/env python

"""
Library for implementation on function/methods on how to compute errors,
specifically Mean Square Error estimation
"""

import numpy as np


def compute_mse(y_data: np.array, predict_data: np.array):
    """
    Function that computes the MSE given an array of data points and an array
    of predicted data points.
    """
    size_data = np.size(y_data)
    size_prediction = np.size(predict_data)
    # Making sure both arrays have same size
    assert size_data == size_prediction
    # Compute MSE
    addition = np.sum((y_data - predict_data)**2)
    mse = addition/size_data
    return mse
