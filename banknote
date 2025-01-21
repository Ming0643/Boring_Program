# 紅包鈔票張數計算
## variables 向量。可寫入多項金額
## types 向量。鈔票單位
ev <- function(variables, types = c(1000, 100)) {
  len_v <- length(variables) 
  len_t <- length(types)
  output <- matrix(0, len_v, len_t)
  # 排序 types，降序排列
  types <- sort(types, decreasing = TRUE)
  
  # 迭代每個變數
  for (j in 1:len_v) {
    remaining_value <- variables[j]  # 初始的剩餘值是該變數
    # 迭代每個類型
    for (i in 1:len_t) {
      output[j, i] <- remaining_value %/% types[i]
      remaining_value <- remaining_value %% types[i]  # 更新剩餘值
    }
  }
  # 設置列名
  colnames(output) <- as.character(types)
  return(output)
}
