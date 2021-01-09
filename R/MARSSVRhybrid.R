#code for MARS model
MARSSVRhybrid<-function(Data,k,deg,funct="")
{
  Data_org<-as.matrix(Data)
  length_data<-length(Data_org[,1])
  train_size=ceiling(length_data*k)
  train=sample(1:length_data,train_size,replace = F)
  traindata=Data_org[train,]
  testdata=Data_org[-train,]
  Y_train<-traindata[,1]
  X_train<-traindata[,-1]

  train_data<-data.frame(Y_train,X_train)

  model_mars<-earth(Y_train~.,data=train_data,degree=deg)

  Summary_MARS<-summary(model_mars,digit=3)

  imp_variables_MARS<-evimp(model_mars)

  prediction_mars_train<- predict(model_mars,traindata)

  RMSE_insample<-round(mean((Y_train - prediction_mars_train)^2),3)

  MAPE_train=round(100*(mean((abs(Y_train-prediction_mars_train)/Y_train))),3)

  #0utsample
  prediction_out<- predict(model_mars,testdata)

  RMSE_MARS<-round(mean((testdata[,1] - prediction_out)^2),3)

  MAPE_MARS=round(100*(mean((abs(testdata[,1]-prediction_out)/testdata[,1]))),3)


  no_of_selected_var<-length(imp_variables_MARS[,1])

  selected_var_matrix<-matrix(nrow = nrow(X_train),ncol =no_of_selected_var)

  for(i in 1:no_of_selected_var)
  {
    selected_var_matrix<-X_train[,imp_variables_MARS[,1]]
  }
  #SVR
  data_svr<-data.frame(Y_train,selected_var_matrix)

  model_svm <-svm(Y_train ~ ., data=data_svr,kernel=funct)

  Summary_MARS_SVR_hybrid<-summary(model_svm)

  predict_svr_out <- predict(model_svm,testdata)

  RMSE_MARS_SVR_hybrid<-round(mean((testdata[,1]-predict_svr_out)^2),3)

  MAPE_MARS_SVR_hybrid=round(100*(mean((abs(testdata[,1]-predict_svr_out)/testdata[,1]))),3)

  prediction_accuracy=data.frame(RMSE_MARS_SVR_hybrid,RMSE_MARS,MAPE_MARS_SVR_hybrid,MAPE_MARS)

  combined<-list(Summary_MARS,imp_variables_MARS,Summary_MARS_SVR_hybrid,prediction_accuracy)

  return(combined)
}


