print(getwd())
setwd("D:/Paper/Boreal_forest/Deeplearning")

# 导入所需的库
library(randomForest)

# 读取CSV文件
data <- read.csv("Training_B_VIs2.csv")

# 将数据集拆分为训练集和测试集
set.seed(123)  # 设置随机种子以确保可重复性
train_indices <- sample(1:nrow(data), 0.7*nrow(data))  # 70% 数据作为训练集
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# 定义自变量和因变量
x <- train_data[, -1]  # 自变量，排除第一列作为因变量
y <- train_data[, 1]   # 因变量，第一列

# 训练随机森林模型
rf_model <- randomForest(x, y, ntree =5000)
rf_model


# 预测测试集数据
predicted <- predict(rf_model, test_data[, -1])

# 计算精度评价指标（例如，均方根误差）
rmse <- sqrt(mean((test_data[, 1] - predicted)^2))
r_squared <- cor(test_data[, 1], predicted)^2

# 输出评价结果
cat("RMSE:", rmse, "\n")
cat("R2:", r_squared, "\n")


# 特征重要性分析
importance <- importance(rf_model)
importance

varImpPlot(rf_model)

result <- data.frame(Predicted = predicted, Actual = test_data[, 1])

# 导出到CSV文件
write.csv(result, file = "predictions_3rf.csv", row.names = FALSE)

