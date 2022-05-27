# Examen-Regresion-Avanzada
Los ensayos clínicos fase I son experimentos con personas o animales en donde el objetivo es determinar la dosis más alta de una droga que se le puede dar a un paciente. 

Considera el siguiente ensayo clínico fase 1: 20 animales son expuestos a 4 distintos niveles de droga, asignados de manera uniforme, i.e., cada dosis se probó en 5 animales.
Los niveles de droga en escala logaritmica son (-0.86,-0.30,-0.05,0.73). Al final del
estudio se registró el número de animales muertos, que para cada dosis fueron (0,1,3,5)
respectivamente.

La idea es modelar el número de animales muertos (Y) en función de la cantidad de droga (en escala logarítmica) (X) y el número de individuos expuestos o tratados (m). Considera un modelo de regresión lineal generalizado binomial con liga g: [0,1] → R
