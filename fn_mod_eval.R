eval_model <- function(mod, test_dataframe) {
  fn_prediction = predict(mod,test_dataframe)
  index = which(fn_prediction>=0.5)
  fn_prediction[index] = 1
  fn_prediction[-index] = 0
  fn_cx <- table(fn_prediction, test_dataframe$is_success)
  
  fn_precision <- fn_cx[2,2]/(fn_cx[2,2] + fn_cx[2,1])
  fn_recall <- fn_cx[2,2]/(fn_cx[2,2] + fn_cx[1,2])
  fn_f1 <- 2 * fn_precision * fn_recall / (fn_precision + fn_recall)
  
  print('Confusion Matrix shown below:')
  print(fn_cx)
  print(paste('Precision: ', round(100*fn_precision, 4), '%'))
  print(paste('Recall: ', round(100*fn_recall, 4), '%'))
  print(paste('F1 Score: ', round(100*fn_f1, 4),'%'))
  
  return(fn_cx)
}