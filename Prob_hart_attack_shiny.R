heart <- HA

head(heart)
str(heart)
summary(heart)

colSums(is.na(heart))

heart$sex       <- factor(heart$sex, levels = c(0,1), labels = c("Female","Male"))
heart$fbs       <- factor(heart$fbs, levels = c(0,1), labels = c("No","Yes"))
heart$exang     <- factor(heart$exang, levels = c(0,1), labels = c("No","Yes"))
heart$condition <- factor(heart$condition, levels = c(0,1), labels = c("NoDisease","Disease"))

heart$cp       <- factor(heart$cp)
heart$restecg  <- factor(heart$restecg)
heart$slope    <- factor(heart$slope)
heart$thal     <- factor(heart$thal)
heart$ca       <- factor(heart$ca)

str(heart)

table(heart$condition)
prop.table(table(heart$condition))

boxplot(age ~ condition, data = heart,
        col = c("lightblue","salmon"),
        main = "Age vs Heart Disease",
        ylab = "Age")


table(heart$sex, heart$condition)

barplot(table(heart$sex, heart$condition),
        beside = TRUE,
        col = c("skyblue","orange"),
        legend = TRUE,
        main = "Heart Disease by Sex",
        ylab = "Count")


prop.table(table(heart$sex, heart$condition), margin=1)

boxplot(chol ~ condition, data = heart,
        col = c("lightgreen","pink"),
        main = "Cholesterol vs Heart Disease",
        ylab = "Cholesterol")

boxplot(thalach ~ condition, data = heart,
        col = c("lightyellow","lightcoral"),
        main = "Max Heart Rate vs Heart Disease",
        ylab = "Max Heart Rate")

table(heart$exang, heart$condition)
barplot(table(heart$exang, heart$condition),
        beside = TRUE,
        col = c("cyan","magenta"),
        legend = TRUE,
        main = "Exercise-Induced Angina vs Heart Disease")



t.test(chol ~ condition, data = heart)
t.test(thalach ~ condition, data = heart)

chisq.test(table(heart$sex, heart$condition))
chisq.test(table(heart$exang, heart$condition))

# Logistic Regression Model
model <- glm(condition ~ age + sex + chol + thalach + exang,
             data = heart, family = binomial)

summary(model)

exp(coef(model))


# Predicted probabilities
pred_prob <- predict(model, type = "response")

# Convert to class labels
pred_class <- ifelse(pred_prob > 0.5, "Disease", "NoDisease")

# Confusion matrix
table(Predicted = pred_class, Actual = heart$condition)

# Accuracy
mean(pred_class == heart$condition)


# Load libraries
library(shiny)

# ---------------------------
# Data preparation
# ---------------------------
heart$sex       <- factor(heart$sex, levels = c(0,1), labels = c("Female","Male"))
heart$cp        <- factor(heart$cp, levels = 0:3)
heart$fbs       <- factor(heart$fbs, levels = c(0,1), labels = c("No","Yes"))
heart$restecg   <- factor(heart$restecg, levels = 0:2)
heart$slope     <- factor(heart$slope, levels = 0:2)
heart$ca        <- factor(heart$ca, levels = 0:3)
heart$thal      <- factor(heart$thal, levels = 0:2, labels = c("Normal","Fixed Defect","Reversable Defect"))
heart$exang     <- factor(heart$exang, levels = c(0,1), labels = c("No","Yes"))
heart$condition <- factor(heart$condition, levels = c(0,1), labels = c("No Disease","Disease"))

# Logistic regression model
model <- glm(
  condition ~ age + sex + cp + trestbps + chol + fbs +
    restecg + thalach + exang + oldpeak + slope + ca + thal,
  data = heart,
  family = binomial
)

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  titlePanel("Heart Disease Risk Prediction Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Instructions for Doctors"),
      tags$ul(
        tags$li("Enter patient name and title."),
        tags$li("Fill in all patient information accurately."),
        tags$li("Select the correct options for categorical inputs."),
        tags$li("Click 'Predict Risk' to calculate probability of heart disease."),
        tags$li("Click 'Clear All' to reset all inputs.")
      ),
      br(),
      h3("Patient Information"),
      textInput("patient_name", "Patient Name", ""),
      selectInput("patient_title", "Title", choices = c("Mr.","Ms.","Mrs.")),
      numericInput("age", "Age (years)", 50, min = 20, max = 90),
      selectInput("sex", "Sex", levels(heart$sex)),
      selectInput("cp", "Chest Pain Type (0=typical,1=atypical,2=non-anginal,3=asymptomatic)", levels(heart$cp)),
      numericInput("trestbps", "Resting Blood Pressure (mm Hg)", 120),
      numericInput("chol", "Serum Cholesterol (mg/dl)", 200),
      selectInput("fbs", "Fasting Blood Sugar > 120 mg/dl", levels(heart$fbs)),
      selectInput("restecg", "Resting ECG Results (0=normal,1=ST-T abnormal,2=LVH)", levels(heart$restecg)),
      numericInput("thalach", "Maximum Heart Rate Achieved", 150),
      selectInput("exang", "Exercise Induced Angina", levels(heart$exang)),
      numericInput("oldpeak", "ST Depression Induced by Exercise", 1.0, step = 0.1),
      selectInput("slope", "Slope of Peak Exercise ST Segment (0=upsloping,1=flat,2=downsloping)", levels(heart$slope)),
      selectInput("ca", "Number of Major Vessels (0-3) colored by fluoroscopy", levels(heart$ca)),
      selectInput("thal", "Thalassemia Type", levels(heart$thal)),
      br(),
      actionButton("predict", "Predict Risk", class = "btn-danger"),
      actionButton("clear", "Clear All", class = "btn-secondary")
    ),
    
    mainPanel(
      br(),
      h2("Prediction Result", align = "center"),
      br(),
      uiOutput("probabilityBox"),
      uiOutput("riskBox"),
      br(),
      uiOutput("conclusionBox")
    )
  )
)

# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {
  
  # Reactive newdata for prediction
  newdata <- eventReactive(input$predict, {
    data.frame(
      age = input$age,
      sex = factor(input$sex, levels = levels(heart$sex)),
      cp = factor(input$cp, levels = levels(heart$cp)),
      trestbps = input$trestbps,
      chol = input$chol,
      fbs = factor(input$fbs, levels = levels(heart$fbs)),
      restecg = factor(input$restecg, levels = levels(heart$restecg)),
      thalach = input$thalach,
      exang = factor(input$exang, levels = levels(heart$exang)),
      oldpeak = input$oldpeak,
      slope = factor(input$slope, levels = levels(heart$slope)),
      ca = factor(input$ca, levels = levels(heart$ca)),
      thal = factor(input$thal, levels = levels(heart$thal))
    )
  })
  
  # Probability display with 5 stages
  output$probabilityBox <- renderUI({
    req(newdata())
    prob <- predict(model, newdata(), type = "response")
    color <- if(prob < 0.2) "green"
    else if(prob < 0.4) "lightgreen"
    else if(prob < 0.6) "orange"
    else if(prob < 0.8) "orangered"
    else "red"
    
    HTML(paste0(
      "<div style='text-align:center;
                  font-size:50px;
                  font-weight:bold;
                  color:", color, ";'>",
      round(prob * 100, 2), "%</div>"
    ))
  })
  
  # Risk label with 5 stages
  output$riskBox <- renderUI({
    req(newdata())
    prob <- predict(model, newdata(), type = "response")
    if(prob < 0.2) label <- "VERY LOW RISK"
    else if(prob < 0.4) label <- "LOW RISK"
    else if(prob < 0.6) label <- "MODERATE RISK"
    else if(prob < 0.8) label <- "HIGH RISK"
    else label <- "VERY HIGH RISK"
    
    color <- if(prob < 0.2) "green"
    else if(prob < 0.4) "lightgreen"
    else if(prob < 0.6) "orange"
    else if(prob < 0.8) "orangered"
    else "red"
    
    HTML(paste0(
      "<div style='text-align:center;
                  font-size:28px;
                  font-weight:bold;
                  color:", color, ";'>",
      label, "</div>"
    ))
  })
  
  # Strong medical conclusion with 5 stages
  output$conclusionBox <- renderUI({
    req(newdata())
    prob <- predict(model, newdata(), type = "response")
    patient <- input$patient_name
    title <- input$patient_title
    if (patient == "") patient <- "The patient"
    else patient <- paste(title, patient)
    
    message <- ""
    
    if(prob < 0.2) {
      message <- paste0(
        patient, "'s risk of heart disease is VERY LOW.\n",
        "Advice: Routine checkups once a year.\n",
        "Good foods: Fresh fruits, vegetables, whole grains, lean proteins.\n",
        "Avoid: Processed foods, high sugar, fried foods, excessive salt.\n",
        "Lifestyle: Regular exercise, maintain healthy weight, avoid smoking.\n",
        "Tests: Basic blood tests, blood pressure monitoring."
      )
    } else if(prob < 0.4) {
      message <- paste0(
        patient, "'s risk of heart disease is LOW.\n",
        "Advice: Regular monitoring every 6–12 months.\n",
        "Good foods: High fiber diet, healthy fats (olive oil, nuts), lean proteins.\n",
        "Avoid: Red meat, sugary drinks, excessive salt, fried foods.\n",
        "Lifestyle: Moderate exercise 4–5 times/week, stress management.\n",
        "Tests: ECG, lipid profile, blood pressure checks."
      )
    } else if(prob < 0.6) {
      message <- paste0(
        patient, "'s risk of heart disease is MODERATE.\n",
        "Advice: Monitor health closely and consult a doctor regularly.\n",
        "Good foods: Vegetables, fruits, whole grains, fish, low-fat dairy.\n",
        "Avoid: High-fat foods, processed meat, alcohol in excess.\n",
        "Lifestyle: Daily exercise, reduce stress, maintain ideal weight.\n",
        "Tests: ECG, Echocardiogram, stress test, lipid panel, blood sugar tests."
      )
    } else if(prob < 0.8) {
      message <- paste0(
        patient, "'s risk of heart disease is HIGH.\n",
        "Advice: Immediate consultation with a cardiologist is recommended.\n",
        "Good foods: Low-salt, high fiber, vegetables, fruits, lean proteins.\n",
        "Avoid: Fried foods, sugary foods, high-fat diets, alcohol.\n",
        "Lifestyle: Daily physical activity, strict weight control, stop smoking.\n",
        "Tests: Full cardiac evaluation including ECG, echocardiogram, stress tests, blood work."
      )
    } else {
      message <- paste0(
        patient, "'s risk of heart disease is VERY HIGH.\n",
        "Advice: Urgent cardiology assessment is required.\n",
        "Good foods: Heart-healthy diet strictly (vegetables, fruits, whole grains).\n",
        "Avoid: All unhealthy foods, high salt, high sugar, saturated fat, alcohol, smoking.\n",
        "Lifestyle: Strict physical activity under supervision.\n",
        "Tests: Complete cardiac workup including ECG, Echocardiogram, Coronary Angiography, full blood panel."
      )
    }
    
    HTML(paste0(
      "<div style='text-align:left;
                  font-size:18px;
                  font-weight:bold;
                  color:black;
                  line-height:1.5;'>",
      gsub("\n","<br/>", message),
      "</div>"
    ))
  })
  
  # Clear all inputs and outputs
  observeEvent(input$clear, {
    updateTextInput(session, "patient_name", value = "")
    updateSelectInput(session, "patient_title", selected = "Mr.")
    updateNumericInput(session, "age", value = 50)
    updateSelectInput(session, "sex", selected = "Female")
    updateSelectInput(session, "cp", selected = 0)
    updateNumericInput(session, "trestbps", value = 120)
    updateNumericInput(session, "chol", value = 200)
    updateSelectInput(session, "fbs", selected = "No")
    updateSelectInput(session, "restecg", selected = 0)
    updateNumericInput(session, "thalach", value = 150)
    updateSelectInput(session, "exang", selected = "No")
    updateNumericInput(session, "oldpeak", value = 1.0)
    updateSelectInput(session, "slope", selected = 0)
    updateSelectInput(session, "ca", selected = 0)
    updateSelectInput(session, "thal", selected = "Normal")
    
    output$probabilityBox <- renderUI({NULL})
    output$riskBox <- renderUI({NULL})
    output$conclusionBox <- renderUI({NULL})
  })
}

# ---------------------------
# Run App
# ---------------------------
shinyApp(ui = ui, server = server)

