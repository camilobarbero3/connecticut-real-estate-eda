[**Read in English**](#-english-version) \| [**Leer en Español**](#-versión-en-español)

------------------------------------------------------------------------

## 🇬🇧 English Version

# Connecticut Real Estate Market: Exploratory Data Analysis (EDA)

### 📌 Overview

This project performs an in-depth Exploratory Data Analysis (EDA) and Time Series Forecasting on a dataset of real estate transactions in Connecticut. The main goal is to identify the key drivers of property profitability, understand the impact of macroeconomic events (like the 2008 housing crisis), and discover seasonal trends to optimize buying and selling strategies.

### 🎯 Objectives

- **Profitability Analysis:** Identify which cities offer the highest return on investment by calculating the *Spread* (the ratio between Sale Price and Estimated Value).
- **Feature Importance:** Determine whether location or physical property characteristics (e.g., number of rooms, carpet area) have a greater impact on profitability.
- **Macroeconomic Trends:** Analyze historical transaction trends, observing the lasting effects of the 2008 housing market crash and identifying regional gentrification processes.
- **Seasonality:** Discover the best time of the year to buy or sell a property to maximize gains.

### 🛠️ Methodology & Data Cleaning

- **Outlier Treatment:** Addressed extreme values using the Interquartile Range (IQR) method combined with business logic to correct human data entry errors (e.g., correcting decimal placements in tax rates and extraneous zeros in room counts).
- **Missing Value Imputation:** Applied Multiple Imputation (MICE) for highly correlated numerical variables, deterministic conditional imputation for perfectly associated categorical variables (Cramer's V), and linear interpolation for time-series data.
- **Statistical Modeling:** Built an optimized linear regression model with cross-validation to determine feature importance, and utilized ARIMA models and Augmented Dickey-Fuller (ADF) tests for time-series forecasting.

### 📊 Key Findings

- **Location is King:** Profitability is heavily influenced by locality (100% relative importance in the regression model). A "poor-quality" house in a premium neighborhood (like Greenwich) is statistically more profitable than a "high-quality" house in a lower-tier neighborhood.
- **Market Recovery & Gentrification:** Time series analysis revealed a clear inflation of property prices during the 2008 crisis, followed by a market correction until 2013. A deep dive into regional trends highlighted a continuous non-stationary, upward trend in Bridgeport, suggesting a strong gentrification process.
- **Seasonality Strategy:** The optimal month to **buy** a property is February (lowest spread, often negative at \~-11%), while the best month to **sell** is July (highest spread, reaching up to \~14% above estimated value).

### 📂 Repository Structure

- `/data`: Dataset sourced from Kaggle.
- `/notebooks`: Contains the EDA, data cleaning, and statistical modeling code (`.Rmd` and `.md`).
- `/scripts`: Auxiliary functions and library dependencies (`.R`).
- `/report`: The comprehensive academic report detailing the statistical methods and conclusions (PDF).

------------------------------------------------------------------------

## 🇪🇸 Versión en Español

# Mercado Inmobiliario de Connecticut: Análisis Exploratorio de Datos (EDA)

### 📌 Resumen

Este proyecto realiza un Análisis Exploratorio de Datos (EDA) profundo y pronóstico de Series de Tiempo sobre un conjunto de datos de transacciones inmobiliarias en Connecticut. El objetivo principal es identificar los factores clave que impulsan la rentabilidad de las propiedades, comprender el impacto de eventos macroeconómicos (como la crisis inmobiliaria de 2008) y descubrir tendencias estacionales para optimizar estrategias de compra y venta.

### 🎯 Objetivos

- **Análisis de Rentabilidad:** Identificar qué ciudades ofrecen el mayor retorno de inversión calculando el *Spread* (la relación entre el Precio de Venta y el Valor Estimado).
- **Importancia de Variables:** Determinar si la ubicación o las características físicas de la propiedad (ej. cantidad de habitaciones, superficie) tienen un mayor impacto en la rentabilidad.
- **Tendencias Macroeconómicas:** Analizar las tendencias históricas de las transacciones, observando los efectos a largo plazo de la crisis del mercado inmobiliario de 2008 e identificando procesos de gentrificación regional.
- **Estacionalidad:** Descubrir la mejor época del año para comprar o vender una propiedad para maximizar las ganancias.

### 🛠️ Metodología y Limpieza de Datos

- **Tratamiento de Outliers:** Se abordaron los valores extremos utilizando el método del Rango Intercuartílico (IQR) combinado con lógica de negocio para corregir errores humanos de tipeo (ej. corrección de decimales en tasas impositivas y ceros de más en cantidad de cuartos).
- **Imputación de Valores Faltantes:** Se aplicó Imputación Múltiple (MICE) para variables numéricas correlacionadas, imputación condicional determinística para variables categóricas perfectamente asociadas (V de Cramer) e interpolación lineal para datos temporales.
- **Modelado Estadístico:** Se construyó un modelo de regresión lineal optimizado con validación cruzada para determinar la importancia de las variables, y se utilizaron modelos ARIMA y el test Aumentado de Dickey-Fuller (ADF) para el pronóstico de series de tiempo.

### 📊 Principales Hallazgos

- **La Ubicación es Clave:** La rentabilidad está fuertemente influenciada por la localidad (100% de importancia relativa en el modelo). Una casa estructuralmente "mala" en un barrio premium (como Greenwich) es estadísticamente más rentable que una casa "buena" en un barrio de menor categoría.
- **Recuperación del Mercado y Gentrificación:** El análisis de series temporales reveló una clara inflación de precios durante la crisis de 2008, seguida de una corrección del mercado hasta 2013. Un análisis regional profundo destacó una tendencia alcista, no estacionaria y continua en Bridgeport, lo que sugiere un fuerte proceso de gentrificación.
- **Estrategia Estacional:** El mes óptimo para **comprar** una propiedad es febrero (spread más bajo, a menudo negativo en \~-11%), mientras que el mejor mes para **vender** es julio (spread más alto, alcanzando hasta un \~14% por encima del valor estimado).

### 📂 Estructura del Repositorio

- `/data`: Conjunto de datos extraído de Kaggle.
- `/notebooks`: Contiene el código de limpieza de datos, EDA y modelado estadístico (`.Rmd` y archivo `.md`).
- `/scripts`: Funciones auxiliares y dependencias de librerías (`.R`).
- `/report`: El informe académico completo detallando los métodos y conclusiones (PDF).
