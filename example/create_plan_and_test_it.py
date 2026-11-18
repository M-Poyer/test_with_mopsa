import time

class abstraction_set():
    def __init__(self, state):
        self.abstraction_tokens = state['tokens']
        self.abstraction_goal = state['goal']
        if state['values'] == []:
            self.abstraction_values = set([state['goal']])
        else:
            self.abstraction_values = set(state['values'])
        self.abstraction_used = state['used'].copy()

    def __repr__(self):
        return f'Values : {self.abstraction_values}, used : {self.abstraction_used}, token : {self.abstraction_tokens}'

    def use_token(self, token):
        if not(token in self.abstraction_tokens):
            raise ValueError("Is not a valid token")
        elif not(self.abstraction_used.count(token) < self.abstraction_tokens.count(token)):
            raise ValueError("Token has already been used")
        elif 0 in self.abstraction_values:      # TODO : à enlever plus tard quand j'ajouterai de l'aléatoire, car j'aurais peut-être besoin des états avants le 0
            raise ValueError("The goal has already been reached")
        else:
            copy_values = self.abstraction_values.copy()
            self.abstraction_values = []

            # On ajoute les additions :
            self.abstraction_values += [i + token for i in copy_values if i!= 0]

            # On ajoute les soustractions :
            self.abstraction_values += [abs(i - token) for i in copy_values if i != 0]

            # On ajoute les divisions :
            self.abstraction_values += [max(i,token)//min(i,token) for i in copy_values if (max(i,token)%min(i,token)==0) and (i!=0) and (i!=1) and (token!= 1)]

            # On ajoute les multiplications :
            self.abstraction_values += [i*token for i in copy_values if (i!=0) and (i!=1) and (token!= 1)]

            # On enlève les doublons
            self.abstraction_values = set(self.abstraction_values)

            # update the used tokens
            self.abstraction_used.append(token)


state = {
    'tokens': [1, 75, 3, 100, 2, 10, 7],
    'goal': 620,
    'values': [],
    'used': []
}

test = abstraction_set(state)
print(test)
test.use_token(10)
print(test)
test.use_token(7)
print(test)
test.use_token(1)
print(test)
test.use_token(2)
print(test)
test.use_token(3)
print(test)
test.use_token(75)
print(test)
# test.use_token(100)
# print(test)

print(" ")
print("Partie 2 : création du graphe")

import networkx as nx
import matplotlib.pyplot as plt
from collections import deque

def compte_est_bon_graph_solver_with_visual(initial_state, show_graph=False):
    root = abstraction_set(initial_state)
    G = nx.DiGraph()

    liste_nodes_with_value = [[] for k in range(1000)]

    visited = set()
    queue = deque()
    queue.append((root, [], "root"))  # (state, path, node_id)
    id_counter = 1
    solution_path_ids = []
    test_solution = []

    node_map = {}  # pour retrouver les noms de nœuds facilement

    while queue:
        current_state, path, node_id = queue.popleft()
        state_key = (frozenset(current_state.abstraction_values), tuple(sorted(current_state.abstraction_used)))

        if state_key in visited:
            continue
        visited.add(state_key)

        # Ajouter le nœud au graphe
        label = f"{0 in current_state.abstraction_values}\nUsed: {current_state.abstraction_used}"
        G.add_node(node_id, label=label, values=current_state.abstraction_values, used=current_state.abstraction_used) # profondeur = len(current_state.abstraction_used)
        for i in current_state.abstraction_values:
            if i<1000:
                liste_nodes_with_value[i].append(node_id)

        if 0 in current_state.abstraction_values:          # 0 pour le faire en backward
            solution_path_ids = path + [node_id]
            test_solution.append(solution_path_ids.copy())
            # break

        for token in current_state.abstraction_tokens:
            try:
                # Création d’un nouvel état propre (copie indépendante)
                new_state_dict = {
                    'tokens': current_state.abstraction_tokens.copy(),
                    'goal': current_state.abstraction_goal,
                    'values': list(current_state.abstraction_values),
                    'used': current_state.abstraction_used.copy()
                }
                new_state = abstraction_set(new_state_dict)

                # Application du token
                new_state.use_token(token)

                # Génération d’un nouvel identifiant unique
                new_id = f"n{id_counter}"
                id_counter += 1

                # Ajout dans la file d’attente
                queue.append((new_state, path + [node_id], new_id))

                # Ajout de l’arête au graphe
                G.add_edge(node_id, new_id, label=str(token))

            except ValueError:
                continue

    print(G, "et il y a des solutions", len(test_solution))
    print("Chemin d'une solution :")  # TODO regarder s'il s'arrête une fois la solution trouvée
    for node in solution_path_ids:
        print(f"Noeud {node} : {G.nodes[node]}")

    print("l'ensemble des nœuds qui ont une solution", liste_nodes_with_value[0])

    if show_graph:
        # Dessin du graphe
        pos = nx.spring_layout(G, seed=42)
        labels = nx.get_node_attributes(G, 'label')
        edge_labels = nx.get_edge_attributes(G, 'label')

        plt.figure(figsize=(18, 10))
        nx.draw(G, pos, with_labels=False, node_size=1800, node_color="lightblue", edge_color='gray')
        nx.draw_networkx_labels(G, pos, labels, font_size=8)
        nx.draw_networkx_edge_labels(G, pos, edge_labels=edge_labels, font_size=8)

        # Colorer le chemin solution
        if solution_path_ids:
            path_edges = [(solution_path_ids[i], solution_path_ids[i+1]) for i in range(len(solution_path_ids)-1)]
            nx.draw_networkx_edges(G, pos, edgelist=path_edges, edge_color='red', width=2)
            print("✅ Solution trouvée !")
        else:
            print("❌ Pas de solution trouvée.")

        plt.title("Graphe du problème Le Compte est Bon")
        plt.axis('off')
        plt.show()

    return G, liste_nodes_with_value

# Ajouter dynamique toutes les secondes une opération.

state = {
    'tokens': [1, 75, 3, 100, 2, 10, 7],
    'goal': 620,
    'values': [],
    'used': []
}

deb = time.time()
G, liste_nodes_with_value = compte_est_bon_graph_solver_with_visual(state)
fin = time.time()

print(fin-deb)

print(" ")
print("Partie 3 : Solveur dynamique")

import time
from collections import deque

def dynamic_solver_with_delays(graph, launch_schedule, root='n0', max_steps=30):
    """
    launch_schedule : list of tuples (time_to_start, node_id)
    """
    active_paths = []  # list of tuples: (start_time, current_path)
    active_value = []  # list of values
    finished_paths = []

    for t in range(max_steps):
        print(f"\nTemps {t}s")

        # → Lancer les nouveaux chemins prévus à ce temps
        for launch_time, node in launch_schedule:
            if launch_time == t:
                print(f"🚀 Lancement du chemin depuis {node}")
                active_paths.append((t, [node]))
                active_value.append(0)

        new_active_paths = []
        new_active_values = []

        count = 0
        for start_time, path in active_paths:
            current = path[-1]
            print(f"  🔹 Chemin {path[0]} (lancé à t={start_time}) : {path}")
            print(f"    ↪️  {current} : {graph.nodes[current]['used']}")

            if current == "root":
                print(f"    ✅ Chemin arrivé à la racine : {path}")
                finished_paths.append(path)
                count += 1
                continue

            for parent in graph.predecessors(current): # TODO j'ai pas de cycles dans mon graphe... Pourquoi nécessaire ?? Si je le mets en commentaire résultats différents...
                                                        # Ca ne marche que parce que j'ai qu'un parent par nœuds !! À corriger à l'avenir
                if parent not in path:  # éviter les cycles
                    new_active_paths.append((start_time, path + [parent]))
                    node_parent = parent

            if active_value[count]+graph.nodes[current]['used'][-1] in graph.nodes[node_parent]['values']:
                new_active_values.append(active_value[count] + graph.nodes[current]['used'][-1])
                print(f"    ↪️  L'opération est {active_value[count]} + {graph.nodes[current]['used'][-1]} = {new_active_values[-1]}")
            elif active_value[count]-graph.nodes[current]['used'][-1] in graph.nodes[node_parent]['values']:
                new_active_values.append(active_value[count] - graph.nodes[current]['used'][-1])
                print(f"    ↪️  L'opération est {active_value[count]} - {graph.nodes[current]['used'][-1]} = {new_active_values[-1]}")
            elif graph.nodes[current]['used'][-1]-active_value[count] in graph.nodes[node_parent]['values']:
                new_active_values.append(graph.nodes[current]['used'][-1] - active_value[count])
                print(f"    ↪️  L'opération est {graph.nodes[current]['used'][-1]} - {active_value[count]} = {new_active_values[-1]}")
            elif (active_value[count] != 0) and (active_value[count]*graph.nodes[current]['used'][-1] in graph.nodes[node_parent]['values']):
                new_active_values.append(active_value[count] * graph.nodes[current]['used'][-1])
                print(f"    ↪️  L'opération est {active_value[count]} * {graph.nodes[current]['used'][-1]} = {new_active_values[-1]}")
            elif (active_value[count]!=0) and (active_value[count]%graph.nodes[current]['used'][-1]==0) and (active_value[count]//graph.nodes[current]['used'][-1] in graph.nodes[node_parent]['values']):
                new_active_values.append(active_value[count] // graph.nodes[current]['used'][-1])
                print(f"    ↪️  L'opération est {active_value[count]} / {graph.nodes[current]['used'][-1]} = {new_active_values[-1]}")
            elif (active_value[count]!=0) and (active_value[count]%graph.nodes[current]['used'][-1]==0) and (graph.nodes[current]['used'][-1]//active_value[count] in graph.nodes[node_parent]['values']):
                new_active_values.append(graph.nodes[current]['used'][-1] // active_value[count])
                print(f"    ↪️  L'opération est {graph.nodes[current]['used'][-1]} / {active_value[count]} = {new_active_values[-1]}")
            else:
                print(f"    ↪️  Euh.... je sais pas à regarder !!!!!!")
                print(active_value, count)

            count += 1

        # Mettre à jour les chemins actifs
        active_paths = new_active_paths
        active_value = new_active_values
        time.sleep(1)

        if len(finished_paths) == len(launch_schedule):
            print("🛑 Tous les chemins sont terminés.")
            break

    return finished_paths

# Liste des (temps, noeud de départ)
# il faut que je choisisse les bons
# launches = [
#     (0, 'n705'),
#     (1, 'n709'),
#     (3, 'n830'),
#     (5, 'n834'),
# ]

def get_used_window(node, t_start, t_end):
    """Extrait les tokens utilisés entre les étapes t_start et t_end à partir de la fin du chemin jusqu’à la racine"""
    used_tokens = set()
    for i in range(t_start, min(t_end, len(path))):
        used_tokens.update(nodes[path[i]]['state'].abstraction_used)
    return used_tokens


def compute_launches(graph, times, liste_nodes_with_value):
    launches = []
    selected_nodes = []

    for i, t_i in enumerate(times):
        for node_candidate in liste_nodes_with_value:

            valid = True

            # On compare avec chaque nœud déjà sélectionné
            # print("nœud candidat", node_candidate)
            for j, (t_j, node_j) in enumerate(launches):
                # Fenêtre de nj dans l’intervalle qui chevauchera avec ni
                delta = t_i - t_j
                # used_j = get_used_window(graph, node_j, delta , delta + 1)
                # print(graph.nodes[node_j])
                # print(graph.nodes[node_j]['used'])
                for k in range(delta, min(delta + 2, len(graph.nodes[node_j]['used']))):        # TODO: vérifier les bornes !! Il faut tout inverser (on regarde la liste a partir ed la fin)
                    # print(k, k+1, k-delta, k-delta+1)
                    # print(-k,-k-2, -k+delta,-k+delta-2)alculs")
                    # print(graph.nodes[node_candidate]['used'], graph.nodes[node_j]['used'])
                    if -k+delta == 0:
                        # print(graph.nodes[node_candidate]['used'][-k+delta-2:], graph.nodes[node_j]['used'][-k-2:-k])
                        if set(graph.nodes[node_candidate]['used'][-k+delta-2:]) & set(graph.nodes[node_j]['used'][-k-2:-k]):
                            valid = False
                            break
                    else:
                        # print(graph.nodes[node_candidate]['used'][-k+delta-2:-k+delta], graph.nodes[node_j]['used'][-k-2:-k])
                        if set(graph.nodes[node_candidate]['used'][-k+delta-2:-k+delta]) & set(graph.nodes[node_j]['used'][-k-2:-k]):
                            valid = False
                            break

            if valid:
                print("On ajoute le nœud", node_candidate, t_i, graph.nodes[node_candidate]['used'])
                launches.append((t_i, node_candidate))
                selected_nodes.append(node_candidate)
                break  # on passe au temps suivant

    return launches

times = [0, 1, 3, 5]
launches = compute_launches(G, times, liste_nodes_with_value[0])
print("Launches trouvés :", launches)

paths = dynamic_solver_with_delays(G, launches, root='n0', max_steps=20)

print(" ")
print("4. Trouver des alternatives a de mauvais calculs")

import random
random.seed(42)

def dynamic_solver_with_delays(graph, launch_schedule, root='n0', max_steps=30):
    """
    launch_schedule : list of tuples (time_to_start, node_id)
    """
    active_paths = []  # list of tuples: (start_time, current_path)
    active_value = []  # list of values
    finished_paths = []

    for t in range(max_steps):
        print(f"\nTemps {t}s")

        # → Lancer les nouveaux chemins prévus à ce temps
        for launch_time, node in launch_schedule:
            if launch_time == t:
                print(f"🚀 Lancement du chemin depuis {node}")
                active_paths.append((t, [node]))
                active_value.append(0)

        new_active_paths = []
        new_active_values = []

        count = 0
        for start_time, path in active_paths:
            current = path[-1]
            print(f"  🔹 Chemin {path[0]} (lancé à t={start_time}) : {path}")
            print(f"    ↪️  {current} : {graph.nodes[current]['used']}")

            if current == "root":
                print(f"    ✅ Chemin arrivé à la racine : {path}")
                finished_paths.append(path)
                count += 1
                continue

            for parent in graph.predecessors(current): # TODO j'ai pas de cycles dans mon graphe... Pourquoi nécessaire ?? Si je le mets en commentaire résultats différents...
                                                        # Ca ne marche que parce que j'ai qu'un parent par nœuds !! À corriger à l'avenir
                if parent not in path:  # éviter les cycles
                    new_active_paths.append((start_time, path + [parent]))
                    node_parent = parent

            possible_error = 0
            random_error = random.random()
            if random_error < 0.05:
                possible_error = -1
                print(f"    ⚠️  Il y a une erreur négative")
            elif random_error < 0.1:
                possible_error = +1
                print(f"    ⚠️  Il y a une erreur positive")

            if active_value[count]+graph.nodes[current]['used'][-1] in graph.nodes[node_parent]['values']:
                new_active_values.append(active_value[count] + graph.nodes[current]['used'][-1]+possible_error)
                print(f"    ↪️  L'opération est {active_value[count]} + {graph.nodes[current]['used'][-1]} = {new_active_values[-1]}")
            elif active_value[count]-graph.nodes[current]['used'][-1] in graph.nodes[node_parent]['values']:
                new_active_values.append(active_value[count] - graph.nodes[current]['used'][-1]+possible_error)
                print(f"    ↪️  L'opération est {active_value[count]} - {graph.nodes[current]['used'][-1]} = {new_active_values[-1]}")
            elif graph.nodes[current]['used'][-1]-active_value[count] in graph.nodes[node_parent]['values']:
                new_active_values.append(graph.nodes[current]['used'][-1] - active_value[count]+possible_error)
                print(f"    ↪️  L'opération est {graph.nodes[current]['used'][-1]} - {active_value[count]} = {new_active_values[-1]}")
            elif (active_value[count] != 0) and (active_value[count]*graph.nodes[current]['used'][-1] in graph.nodes[node_parent]['values']):
                new_active_values.append(active_value[count] * graph.nodes[current]['used'][-1]+possible_error)
                print(f"    ↪️  L'opération est {active_value[count]} * {graph.nodes[current]['used'][-1]} = {new_active_values[-1]}")
            elif (active_value[count]!=0) and (active_value[count]%graph.nodes[current]['used'][-1]==0) and (active_value[count]//graph.nodes[current]['used'][-1] in graph.nodes[node_parent]['values']):
                new_active_values.append(active_value[count] // graph.nodes[current]['used'][-1]+possible_error)
                print(f"    ↪️  L'opération est {active_value[count]} / {graph.nodes[current]['used'][-1]} = {new_active_values[-1]}")
            elif (active_value[count]!=0) and (active_value[count]%graph.nodes[current]['used'][-1]==0) and (graph.nodes[current]['used'][-1]//active_value[count] in graph.nodes[node_parent]['values']):
                new_active_values.append(graph.nodes[current]['used'][-1] // active_value[count]+possible_error)
                print(f"    ↪️  L'opération est {graph.nodes[current]['used'][-1]} / {active_value[count]} = {new_active_values[-1]}")
            else:
                print(f"    ↪️  Euh.... je sais pas à regarder !!!!!!")
                print(active_value, count)

            # TODO si je veux mettre des conditions
            if random_error != 0 and not(new_active_values[-1] in graph.nodes[node_parent]['values']):
                # print("coucou test", new_active_values[-1], active_paths, current, start_time, t)
                print(f"    ↪️  On a besoin d'un nouveau chemin")
                # print(new_active_paths)
                new_active_paths[-1] = (start_time, path + [find_alternative(new_active_values[-1], active_paths, launch_schedule, graph, current, t, path[0], start_time)])
                print(f"    ↪️  Le nouveau chemin {new_active_paths[-1]}") # et ses tokens {graph.nodes[new_active_paths[-1][1][0]]['used']}

            count += 1

        # Mettre à jour les chemins actifs
        active_paths = new_active_paths
        active_value = new_active_values
        time.sleep(1)

        if len(finished_paths) == len(launch_schedule):
            print("🛑 Tous les chemins sont terminés.")
            break

    return finished_paths

def find_alternative(value, active_paths, launch_schedule, graph, current, actual_time, initial_node, initial_time):
    # TODO : il me manque des listes de paths...
    # Si je prends lauch je n'aurais ps tous les changements passé, mais j'ai tous les chemins
    # Si je prends active_paths je n'ai que les chemins passés, mais pas futurs
    time_new_path = actual_time+1
    if value < 1000:
        nodes_to_test = liste_nodes_with_value[value]
        launches = []
        selected_nodes = []

        # TODO t_i a changer et en particulier ajouter l'offset de notre noœud
        for node_candidate in nodes_to_test:

            valid = True

            # On compare avec chaque nœud déjà sélectionné
            #     print("nœud candidat", node_candidate)
            #     print(graph.nodes[node_candidate]["used"])
            for j, (t_j, node_j) in enumerate(launch_schedule):

                if (t_j==initial_time) and (node_j==initial_node):
                    # c'est le chemin donc ça sert à rien de regarder s'il y a conflit, car on ne va pas le suivre
                    continue

                if node_j == current:
                    # Si le nœud qu'on étudie est le notre c'est inutile
                    continue
                # Fenêtre de nj dans l’intervalle qui chevauchera avec ni
                delta = time_new_path - t_j

                # used_j = get_used_window(graph, node_j, delta , delta + 1)
                #     print(node_j)
                # print(graph.nodes[node_j])
                    # print(graph.nodes[node_j]['used'])
                # TODO : A verifier, il y a un peu de chat GPT
                    # print("Je suis là")
                    # print(delta, len(graph.nodes[node_candidate]['used']))
                    # print(len(graph.nodes[node_candidate]['used'])-delta)
                    # print(len(graph.nodes[node_candidate]['used']) - delta)
                if delta >= 0:
                    for k in range(0, min(len(graph.nodes[node_candidate]['used'])-delta,
                                          len(graph.nodes[node_j]['used'])) - 1):
                        if set(graph.nodes[node_candidate]['used'][-2 - delta - k: -delta - k]) & set(graph.nodes[node_j]['used'][-2 - k: -k]):
                            valid = False
                else:  # delta < 0
                    for k in range(0, min(len(graph.nodes[node_candidate]['used']),
                                          len(graph.nodes[node_j]['used']) + delta) - 1):
                        if set(graph.nodes[node_candidate]['used'][-2 - k: -k]) & set(graph.nodes[node_j]['used'][-2 - k + delta: -k + delta]):
                            valid = False

                # for k in range(delta, min(delta + 2, len(graph.nodes[node_j][
                #                                              'used']))):  # TODO: vérifier les bornes !! Il faut tout inverser (on regarde la liste a partir ed la fin)
                #     print(k, k+1, k-delta, k-delta+1)
                #     print(-k,-k-2, -k+delta,-k+delta-2)
                #     print(graph.nodes[node_candidate]['used'], graph.nodes[node_j]['used'])
                #     if -k + delta == 0:
                #         # print(graph.nodes[node_candidate]['used'][-k+delta-2:], graph.nodes[node_j]['used'][-k-2:-k])
                #         if set(graph.nodes[node_candidate]['used'][-k + delta - 2:]) & set(
                #                 graph.nodes[node_j]['used'][-k - 2:-k]):
                #             valid = False
                #             break
                #     else:
                #         print(graph.nodes[node_candidate]['used'][-k+delta-2:-k+delta], graph.nodes[node_j]['used'][-k-2:-k])
                #         if set(graph.nodes[node_candidate]['used'][-k + delta - 2:-k + delta]) & set(
                #                 graph.nodes[node_j]['used'][-k - 2:-k]):
                #             valid = False
                #             break

            if valid:
                # print("SUPER !!!", node_candidate)
                return node_candidate
                # print("On ajoute le nœud", node_candidate, t_i, graph.nodes[node_candidate]['used'])
                # launches.append((t_i, node_candidate))
                # selected_nodes.append(node_candidate)
                # break  # on passe au temps suivant
        print("🛑 TODO : Mettre une erreur maintenant !!")
        return None

# print(liste_nodes_with_value[71])
times = [0, 1, 3, 5]
launches = compute_launches(G, times, liste_nodes_with_value[0])
print("Launches trouvés :", launches)

paths = dynamic_solver_with_delays(G, launches, root='n0', max_steps=20)
