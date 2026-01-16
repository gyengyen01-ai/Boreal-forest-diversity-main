print(getwd())
setwd("D:/Paper/Boreal_forest/Deeplearning")
library(keras)
library(tidyverse)
library(reticulate)
library(ggplot2)
library(readr)
library(tfruns)

use_python("C:/Users/bfh654/AppData/Local/r-miniconda/python.exe")

# 定义 Inception Time 回归模型
inception_time_regression_model <- function(input_shape) {
  model <- keras_model_sequential()
  model %>%
    layer_conv_1d(filters = 36, kernel_size = 36, activation = "linear", input_shape = input_shape) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_1d(filters = 36, kernel_size = 18, activation = "linear", padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_1d(filters = 36, kernel_size = 18, activation = "linear", padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_1d(filters = 36, kernel_size = 6, activation = "linear", padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_max_pooling_1d(pool_size = 3, strides = 1, padding = "same") %>%
    layer_conv_1d(filters = 36, kernel_size = 6, activation = "linear", padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_conv_1d(filters = 36, kernel_size = 3, activation = "linear", padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_max_pooling_1d(pool_size = 3, strides = 1, padding = "same") %>%
    layer_flatten() %>%
    layer_dense(units = 8, activation = "relu") %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1, activation = "linear")  # 线性激活函数用于回归任务
}

# model <- keras_model_sequential() %>%
#   layer_conv_1d(filters = 32, kernel_size = 39, padding = "same", activation = "relu", input_shape = dim(x_train)[2]) %>%
#   layer_batch_normalization() %>%
#   layer_activation_leaky_relu() %>%
#   layer_conv_1d(filters = 32, kernel_size = 19, padding = "same", activation = "relu") %>%
#   layer_batch_normalization() %>%
#   layer_activation_leaky_relu() %>%
#   layer_conv_1d(filters = 32, kernel_size = 9, padding = "same", activation = "relu") %>%
#   layer_batch_normalization() %>%
#   layer_activation_leaky_relu() %>%
#   layer_conv_1d(filters = 32, kernel_size = 5, padding = "same", activation = "relu") %>%
#   layer_batch_normalization() %>%
#   layer_activation_leaky_relu() %>%
#   layer_conv_1d(filters = 32, kernel_size = 3, padding = "same", activation = "relu") %>%
#   layer_batch_normalization() %>%
#   layer_activation_leaky_relu() %>%
#   layer_global_average_pooling_1d() %>%
#   layer_dense(units = 1)

# 读取CSV数据
data <- read.csv("Training_B_VIs.csv")
data
# 获取特征数据和标签数据
features <- as.matrix(data[, -ncol(data)])  # Assume the last column is the label column
labels <- as.matrix(data[, ncol(data)])

# 数据预处理
# 此处可以对特征数据和标签数据进行预处理，如归一化、标准化等

# 调整特征数据的维度
features <- array_reshape(features, dim = c(dim(features)[1], dim(features)[2], 1))


# 创建模型
input_shape <- dim(features)[-1]  # 输入数据的形状
model <- inception_time_regression_model(input_shape)


# 编译模型
model %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer_adam()
)

# 训练模型
model %>% fit(
  x = features,
  y = labels,
  epochs =100,
  batch_size = 16
)


# 保存模型权重
#model %>% save_weights("model_weights.h5")
save_model_hdf5(model, "model_weights.h5")

# 读取待预测的CSV数据
data_to_predict <- read.csv("predict.csv")

# 获取待预测的特征数据
features_to_predict <- as.matrix(data_to_predict)

# 数据预处理
# 此处可以对特征数据进行预处理，如归一化、标准化等

# 调整特征数据的维度
features_to_predict <- array_reshape(features_to_predict, dim = c(dim(features_to_predict)[1], dim(features_to_predict)[2], 1))
dim(features_to_predict)
# 加载模型权重
model <- load_model_hdf5("model_weights.h5")
#model %>% load_weights("model_weights.h5")

# 进行预测
predictions <- model %>% predict(features_to_predict)

# 将预测结果保存到CSV文件
predictions_df <- data.frame(predictions)

write.csv(predictions_df, "predictions2.csv", row.names = FALSE)

