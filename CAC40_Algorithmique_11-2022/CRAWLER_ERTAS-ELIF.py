import requests
from bs4 import BeautifulSoup
import time
import matplotlib.pyplot as plt
import pandas as pd
from datetime import datetime

#Création du dictionnaire data_carrefour 
data_carrefour = {}
#Création variable temps qui va stocker chaque moment étudié 
temps = []
#Création variable action qui va stocker chaque prix pour un certain temps 
action = []
#Création variable temps qui va stocker les temps en format timestamp
timestamp = []

while len(data_carrefour)<180:
    #J'importe les codes HTML de la page boursorama pour carrefour directement
    try:
        carrefour_re = requests.get("https://www.boursorama.com/cours/1rPCA/")
    except:
        time.sleep(60*5)
    #Je passe les données HTML de carrefour_re en texte
    carrefour_html = BeautifulSoup(carrefour_re.text,"html.parser")
    #Récupération de la date et de l'heure de la dernière transaction
    #On cherche donc la balise span et dont la classe contient tradedate
    update_time = carrefour_html.find_all("span",{"class":"c-instrument c-instrument--tradedate"})
    ## Dans le code HTML, je cherche la balise div contenant le prix de l'action
    carrefour_div = carrefour_html.find_all("div",{"class":"c-faceplate__price"})
    # carrefour_div est donné sous le format "list", je récupére donc chaque élément de format "HTML" 
    #on cherche donc la balise "span" contenant le prix de l'action 
    carrefour_span=[]
    for x in carrefour_div:
        carrefour_span.extend(x.find_all("span",{"class":"c-instrument c-instrument--last"}))
    #On complète donc data_carrefour 
    #On stocke le temps (à l'aide de time.strftime) et le prix à chaque temps t. 
    for cours in carrefour_span:
        for t in update_time:
            now = datetime.now()
            #On ne prend pas en compte les microsecondes donc on les met à 0 
            now = now.replace(microsecond=0) 
            times = datetime.timestamp(now)
            #t = time.strftime("%H:%M:%S",time.localtime()) #"%d %m %Y" si on veut jour/mois/annee etc.
            c = cours.text
            timestamp.append(float(times))
            temps.append(now)
            action.append(float(c))
            data_carrefour[now]=float(c)
    #() est l'intervalle de temps en seconde entre chaque itération de l'algo
    #Dans notre cas, le site boursorama se met à jour toute les 30s 
    #On met donc 30s pour avoir la variation des prix à chaque mise à jour 
    #180 valeurs c'est-à-dire dans un intervalle de 1h30min dans notre cas 
    time.sleep(30) 
        
print(data_carrefour) 
#On affiche sur un graphique la variation du cour en fonction du temps
plt.plot(temps,action,'*r-')

#Création d'un fichier csv où sont stocker les données recoltées par le Crawler
d = {'Timestamp': timestamp,'Date': temps, 'Cours': action}
df = pd.DataFrame(data=d)

carrefourCSV = df.to_csv('carrefourCSV.csv',
                         index = False,
                         columns = ['Timestamp','Date','Cours'],
                         sep = ';'
                         ) 

