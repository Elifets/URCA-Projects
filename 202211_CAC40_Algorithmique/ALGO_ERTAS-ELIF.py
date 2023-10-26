import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from sklearn import linear_model 
from sklearn.model_selection import train_test_split
from datetime import datetime

#On charge les données récoltées et stocker dans un fichier CSV grâce au CRAWLER 
df = pd.read_csv("carrefourCSV.csv",sep=';')

x = pd.DataFrame(df.Timestamp)
y = pd.DataFrame(df.Cour)
     #Visualisation graphique           
fig, ax = plt.subplots()
ax.plot(x, y,'b-*')
ax.set_title('Cours en fonction du temps')

#On charge les données récoltées et stocker dans un fichier CSV grâce au CRAWLER 
dm = pd.read_csv("carrefourCSV.csv",sep=';',index_col='Date',parse_dates=True)
dm = dm['Cour'].to_frame()
#Moyenne mobile simple (SMA)
dm['SMA'] = dm['Cour'].rolling(10).mean() #10 étant le nb d'observation pour calculer 
#                            (c'est pour cela que la courbe commence après les 5 premières minutes)
#Moyenne mobile cumulée (CMA)
dm['CMA'] = dm['Cour'].expanding().mean()
# Moyenne mobile exponentielle (EMA)
dm['EWMA'] = dm['Cour'].ewm(span=10).mean()
    #Visualisation graphique 
dm[['Cour','SMA','CMA','EWMA']].plot(label='DM',figsize=(16, 8))

#Création du modèle 
reg = linear_model.LinearRegression()
#Base de données d'entrainement
x_train, x_test, y_train, y_test = train_test_split(x,y)
#Entrainement du modele 
reg.fit(x_train,y_train)
#Prevision avec modele 
y_pred=reg.predict(x_test)
#Calculer la performance du modele 
print(np.mean((y_test-y_pred)**2)) #Très proche de 0 donc fiable et proche de la réalité

#Affichage des résultats sur un graphique 
fig, axs = plt.subplots(2)
fig.suptitle("Modele lineaire prédiction du cours")
axs[0].plot(x_test,y_test,'r*')
axs[0].set_title('Valeurs test')
axs[1].plot(x,y,'b*')
axs[1].set_title('Valeurs récoltées')
fig.tight_layout()
plt.show()

#Prediction du futur à partir des données du Crawler 
X = df.iloc[len(df)-1,0] #Dernier temps où on a des valeurs extraites  
times=[]
timesd=[]
now = datetime.fromtimestamp(X) #ou datetime.now() si on veut partir de maintenant 
#Attention pour datetime.now() il faut un crawler avec des valeurs assez récentes
# now = now.replace(microsecond=0) 
time = datetime.timestamp(now)
time_date = datetime.fromtimestamp(time)
timesd.append(time_date)
times.append(time)
while len(times)<100:
    time = time + 30 
    times.append(time)
    time_date = datetime.fromtimestamp(time)
    timesd.append(time_date)

#Création d'un fichier csv où sont stocker les données recoltées par le Crawler
d = {'Timestamp': times,'Date':timesd}
dp = pd.DataFrame(data=d)
PrediCarrefourCSV = dp.to_csv('PrediCarrefourCSV.csv',
                          index = False,
                          columns = ['Timestamp','Date'],
                          sep = ';'
                          ) 
        #On y ajoute les valeurs que l'on prédit
dp = pd.read_csv("PrediCarrefourCSV.csv",sep=';')
xx = pd.DataFrame(dp.Timestamp)
yy_pred=reg.predict(xx)
new_dp=dp.assign(Prediction=yy_pred)

print(df) 
print(new_dp)