print(getwd())
setwd("D:/Paper/Boreal_forest/Deeplearning")

# 导入所需的包
library(keras)
library(tidyverse)
library(reticulate)
library(ggplot2)
library(readr)

use_python("C:/Users/bfh654/AppData/Local/r-miniconda/python.exe")

# 设置随机种子
set.seed(42)

# 加载数据
data <- read.csv("Training_B_VIs.csv")  # 替换为你的输入数据的文件路径

# 将数据拆分为特征和标签
features <- as.matrix(data[, -ncol(data)])  # 特征数据，移除最后一列
labels <- data[, ncol(data)]  # 标签数据，最后一列

# 数据预处理
features <- scale(features)  # 特征标准化

# 划分训练集和测试集
train_indices <- sample(1:nrow(data), 0.6 * nrow(data))  # 80% 的数据作为训练集
test_indices <- setdiff(1:nrow(data), train_indices)  # 剩余的数据作为测试集

train_features <- features[train_indices, ]
train_labels <- labels[train_indices]
test_features <- features[test_indices, ]
test_labels <- labels[test_indices]

# 转换为张量
train_features <- array_reshape(train_features, c(dim(train_features), 1))
train_labels <- array_reshape(train_labels, c(length(train_labels), 1))
test_features <- array_reshape(test_features, c(dim(test_features), 1))
test_labels <- array_reshape(test_labels, c(length(test_labels), 1))




# 创建模型
model <- keras_model_sequential()
model %>% 
  layer_conv_1d(filters = 18, kernel_size = 6, activation = "relu", padding = "same", input_shape = c(dim(train_features)[2], 1)) %>%
  layer_max_pooling_1d(pool_size = 2, strides = 1, padding = "same") %>%
  layer_dropout(rate = 0.3) %>%
  layer_conv_1d(filters = 36, kernel_size = 12, activation = "relu", padding = "same") %>%
  layer_max_pooling_1d(pool_size = 2, strides = 1, padding = "same") %>%
  layer_dropout(rate = 0.3) %>%
  layer_conv_1d(filters = 36, kernel_size = 12, activation = "relu", padding = "same") %>%
  layer_max_pooling_1d(pool_size = 2, strides = 1, padding = "same") %>%
  layer_dropout(rate = 0.3) %>%
  layer_conv_1d(filters = 18, kernel_size = 6, activation = "relu", padding = "same") %>%
  layer_max_pooling_1d(pool_size = 2, strides = 1, padding = "same") %>%
  layer_global_average_pooling_1d() %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1, activation = "linear")

# learning_rate <- 0.01
# optimizer <- optimizer_adam(learning_rate = learning_rate)

# 编译模型
model %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer_adam(learning_rate = 0.001),
  metrics = c("mean_absolute_error")
)

# 定义回调函数，用于绘制损失和精度曲线
callbacks <- list(
  callback_model_checkpoint("model_weights.h5", save_best_only = TRUE),
  callback_early_stopping(patience = 100),
  callback_tensorboard(log_dir = "logs")
)

# 训练模型
history <- model %>% fit(
  train_features, train_labels,
  batch_size = 32,
  epochs = 100,
  validation_data = list(test_features, test_labels),
  callbacks = callbacks
)

# 绘制损失和精度曲线
plot(history$metrics$loss, type = "l", col = "blue", xlab = "Epoch", ylab = "Loss", main = "Training Loss")
lines(history$metrics$val_loss, col = "red")
legend("topright", legend = c("Training Loss", "Validation Loss"), col = c("blue", "red"), lty = 1)

plot(history$metrics$mean_absolute_error, type = "l", col = "blue", xlab = "Epoch", ylab = "Mean Absolute Error", main = "Training MAE")
lines(history$metrics$val_mean_absolute_error, col = "red")
legend("topright", legend = c("Training MAE", "Validation MAE"), col = c("blue", "red"), lty = 1)

# 保存模型权重
model %>% save_model_hdf5("model.h5")

# 预测
features_to_predict <- read.csv("predict.csv")  # 替换为你的预测数据的文件路径
features_to_predict <- scale(as.matrix(features_to_predict))  # 预测数据标准化
features_to_predict <- array_reshape(features_to_predict, c(dim(features_to_predict), 1))  # 转换为张量

predictions <- model %>% predict(features_to_predict)

# 将预测结果保存到CSV文件中
result <- data.frame(Predictions = predictions)
write.csv(result, "predictions_OO2.csv", row.names = FALSE)  # 替换为你想保存预测结果的文件路径

