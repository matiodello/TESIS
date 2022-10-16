## PRUEBA DE CORRELACIÓN ENTRE VARIABLES:



# ME GUSTA ----------------------------------------------------------------


# correlación entre volumen de ME GUSTA y comentarios

cor(x=df_blas$me_gusta, y=df_blas$comentarios_23) # 0.5954073


with(df_blas, plot(x=me_gusta, y=comentarios_23, pch=20, col='blue',
                   xlab='enojo', las=1,
                   ylab='n° de comentarios'))




# correlación entre volumen de ME GUSTA y compartidos

cor(x=df_blas$me_gusta, y=df_blas$compartidos) # 0.7508338


with(df_blas, plot(x=me_gusta, y=compartidos, pch=20, col='blue',
                   xlab='enojo', las=1,
                   ylab='n° de comentarios'))



# SIMPATÍA ----------------------------------------------------------------


# Correlación entre SIMPATÍA y n° de comentarios:

cor(x=df_blas$simpatia, y=df_blas$comentarios_23) # 0.2380616


with(df_blas, plot(x=simpatia, y=comentarios_23, pch=20, col='blue',
                   xlab='tristeza', las=1,
                   ylab='n° de comentarios'))


# Prueba de correlación entre SIMPATÍA y n° de compartidos:

cor(x=df_blas$simpatia, y=df_blas$compartidos) #0.1212373


with(df_blas, plot(x=simpatia, y=compartidos, pch=20, col='blue',
                   xlab='tristeza', las=1,
                   ylab='n° de compartidos'))





# ASOMBRO -----------------------------------------------------------------


# correlación entre volumen de ASOMBRO y comentarios

cor(x=df_blas$asombro, y=df_blas$comentarios_23) # 0.3293966


with(df_blas, plot(x=asombro, y=comentarios_23, pch=20, col='blue',
                   xlab='enojo', las=1,
                   ylab='n° de comentarios'))




# correlación entre volumen de ASOMBRO y compartidos

cor(x=df_blas$asombro, y=df_blas$compartidos) # 0.52529


with(df_blas, plot(x=asombro, y=compartidos, pch=20, col='blue',
                   xlab='enojo', las=1,
                   ylab='n° de comentarios'))




# DESCREIMIENTO  ----------------------------------------------------------




# correlación entre volumen de DESCREIMIENTO y comentarios

cor(x=df_blas$descreimiento, y=df_blas$comentarios_23) # 0.4113052


with(df_blas, plot(x=descreimiento, y=comentarios_23, pch=20, col='blue',
                   xlab='enojo', las=1,
                   ylab='n° de comentarios'))




# correlación entre volumen de DESCREIMIENTO y compartidos

cor(x=df_blas$descreimiento, y=df_blas$compartidos) # -0.04533643


with(df_blas, plot(x=descreimiento, y=compartidos, pch=20, col='blue',
                   xlab='enojo', las=1,
                   ylab='n° de comentarios'))


# ENOJO -------------------------------------------------------------------



# correlación entre volumen de ENOJO y comentarios

cor(x=df_blas$enojo, y=df_blas$comentarios_23) # 0.331286


with(df_blas, plot(x=enojo, y=comentarios_23, pch=20, col='blue',
                 xlab='enojo', las=1,
                 ylab='n° de comentarios'))

# correlación entre volumen de ENOJO y compartidos

cor(x=df_blas$enojo, y=df_blas$compartidos) # 0.4905635


with(df_blas, plot(x=enojo, y=compartidos, pch=20, col='blue',
                   xlab='enojo', las=1,
                   ylab='n° de comentarios'))





# TRISTEZA ----------------------------------------------------------------


# Correlación entre TRISTEZA y n° de comentarios:

cor(x=df_blas$tristeza, y=df_blas$comentarios_23) # 0.5753845


with(df_blas, plot(x=tristeza, y=comentarios_23, pch=20, col='blue',
                   xlab='tristeza', las=1,
                   ylab='n° de comentarios'))


# Prueba de correlación entre TRISTEZA y n° de compartidos:

cor(x=df_blas$tristeza, y=df_blas$compartidos) #0.738904


with(df_blas, plot(x=tristeza, y=compartidos, pch=20, col='blue',
                   xlab='tristeza', las=1,
                   ylab='n° de compartidos'))


