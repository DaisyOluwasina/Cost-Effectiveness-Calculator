# 💊 Cost-Effectiveness Calculator

**Author**: Daisy Oluwasina  
**Project Type**: R Shiny Web Application  
**Focus**: Health Economics – Cost-Effectiveness Analysis (CEA)

## 🚀 Overview

This interactive R Shiny app demonstrates a fundamental health economics concept: **Cost-Effectiveness Analysis (CEA)**. It allows users to compare two health interventions – a **standard treatment** and a **new treatment** – based on their **costs** and **QALYs (Quality-Adjusted Life Years)**.

The app calculates the **Incremental Cost-Effectiveness Ratio (ICER)** and plots both treatments on a **Cost-Effectiveness Plane**.

> 🎯 This app was built to showcase my applied skills in health economics, data visualization, and economic evaluation using R.

---

## 🧠 What You Can Do

- Input costs and QALYs for two treatments
- Calculate and display the ICER
- Visualize the result on a cost-effectiveness plane
- Highlight incremental cost and effect in an intuitive plot

---

## 📸 Demo

![App Screenshot](demo_screenshot.png)  
*Coming Soon*

---

## 🛠️ Tech Stack

- [R](https://www.r-project.org/)
- [Shiny](https://shiny.rstudio.com/)
- [ggplot2](https://ggplot2.tidyverse.org/)
- [scales](https://scales.r-lib.org/)

---

## 📦 Installation

1. Clone the repository:

   ```bash
   git clone https://github.com/DaisyOluwasina/Cost-Effectiveness-App.git
   cd Cost-Effectiveness-App
2. Install required packages in R:

    ```bash    
    install.packages(c("shiny", "ggplot2", "scales"))

3. Run the app:
   
   ```bash 
    Shiny::runApp("app.R")
