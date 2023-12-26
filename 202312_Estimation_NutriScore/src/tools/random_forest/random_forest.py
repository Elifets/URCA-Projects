'''
 # @ Author: Jaures Ememaga
 # @ Create Time: 2023-11-23 08:25:37
 # @ Modified by: Jaures Ememaga
 # @ Modified time: 2023-12-05 18:28:40
 # @ Description: This script allows you to launch the construction of Random Forest producer or consumer models
 '''

#### Imports ####
import sys
import os
import pickle
import pandas as pd
import matplotlib.pyplot as plt
import mplcyberpunk
plt.style.use('cyberpunk')

# Move up to the "src" directory
while os.path.basename(os.getcwd()) != "src":
    os.chdir("..")
sys.path.append(os.getcwd())

# Importing the functions of Make_random_Forest
from tools.random_forest.tools_random_forest import hyper_params_search, hyper_params_search_with_feature_elimination

#### Data ####
train, test = pd.read_csv(r'data/train.csv'), pd.read_csv(r'data/test.csv')
# Score digitization
train['score'] = train['score'].map({"E": 0, "D": 1, "C": 2, "B": 3, "A": 4})
test['score'] = test['score'].map({"E": 0, "D": 1, "C": 2, "B": 3, "A": 4})

# Choice on the model to build
choice = input("Build the 'Producers' model (enter P) or build the 'Consumers' model (enter C)")
if choice.lower() == 'p':
    x_train, x_test = train.drop(labels=['score'], axis=1), test.drop(labels=['score'], axis=1)

elif choice.lower() == 'c':
    x_train= train[['energy_100g', 'fat_100g', 'saturated-fat_100g', 'carbohydrates_100g', 'sugars_100g', 'proteins_100g', 'salt_100g']]
    x_test =  test[['energy_100g', 'fat_100g', 'saturated-fat_100g', 'carbohydrates_100g', 'sugars_100g', 'proteins_100g', 'salt_100g']]

else:
    print("Incorrect entry, please enter P or C")

# getting lables for train and test
y_train, y_test = train['score'], test['score']

# We offer a first optimal model with all basic variables according to the producer or consumer profile
best_rf = hyper_params_search(x_train=x_train, y_train=y_train, n_iter=30)

# We are launching the search for an optimal model
king_model, best_features = hyper_params_search_with_feature_elimination(x_train=x_train, y_train=y_train, x_test=x_test,
                                                                        y_test=y_test, actual_model=best_rf,
                                                                        n_iter=30
                                                                        )

# choose if you want to save the trained model. This will replace the previous version
save_choice = input("Do you want to save the trained model? (Y/N). !!! This will replace the previous version")
if save_choice.lower() == 'y':
    if choice.lower() == 'p':
        # Model recording (producer case) optimal in a pickle
        with open(r"tools/random_forest/producer/random_forest_prod.pickle", 'wb') as content:
            pickle.dump(king_model, content)
        # Recording of the list of variables used for the optimal model
        with open(r"tools/random_forest/producer/features_random_forest_prod.txt", 'w', encoding='utf-8') as l:
            for line in best_features:
                l.write(f"{line}\n")
                print("Producer model successfully saved!")
    else:
        # Optimal model (consumer case) model in a pickle
        with open(r"tools/random_forest/consumer/random_forest_conso.pickle", 'wb') as content:
            pickle.dump(king_model, content)
        # Recording of the list of variables used for the optimal model
        with open(r"tools/random_forest/consumer/features_random_forest_conso.txt", 'w', encoding='utf-8') as l:
            for line in best_features:
                l.write(f"{line}\n")

        print("Consumer model successfully saved!")
print("Program completed")
