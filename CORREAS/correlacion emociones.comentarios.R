## PRUEBA DE CORRELACIÓN ENTRE VARIABLES:

# correlación entre volumen de ENOJO y comentarios

cor(x=df_blas$enojo, y=df_blas$comentarios_23) # 0.331286


with(df_blas, plot(x=enojo, y=comentarios_23, pch=20, col='blue',
                 xlab='enojo', las=1,
                 ylab='n° de comentarios'))


# Correlación entre TRISTEZA y n° de comentarios:

cor(x=df_blas$tristeza, y=df_blas$comentarios_23) # 0.5753845


with(df_blas, plot(x=tristeza, y=comentarios_23, pch=20, col='blue',
                   xlab='tristeza', las=1,
                   ylab='n° de comentarios'))
