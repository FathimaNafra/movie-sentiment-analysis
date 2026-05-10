library(keras)

#Load Dataset
imdb <-dataset_imdb(num_words=10000)

x_train <- imdb$train$x
y_train <- imdb$train$y

x_test <- imdb$test$x
y_test <- imdb$test$y

#pad sequence
x_train <- pad_sequences(x_train, maxlen=200)
x_test <- pad_sequences(x_test, maxlen=200)

# Convert to numeric arrays
x_train <- as.array(x_train)
x_test <- as.array(x_test)
y_train <- array(as.numeric(y_train), dim = c(length(y_train), 1))
y_test <- array(as.numeric(y_test), dim = c(length(y_test), 1))

#build model
model <- keras_model_sequential()

model$add(layer_embedding(
    input_dim=10000,
    output_dim=32
))

model$add(layer_global_average_pooling_1d())

model$add(layer_dense(units=16, activation="relu"))

model$add(layer_dense(units=1, activation="sigmoid"))


#Compile model

model$compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = list("accuracy")
)

#Train moodel

model$fit(
    x = x_train,
    y = y_train,
    epochs = 5L,
    batch_size = 512L,
    validation_split = 0.2,
    verbose = 1
)

#Evaluate
model$evaluate(x = x_test, y = y_test)

#save model
model$save_weights("Sentiment_model.weights.h5")
