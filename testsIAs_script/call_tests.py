# To call this file from the terminal launch the following command from the PrologPuissance4 directory:
# python3 testsIAs_script/call_tests.py
# If you are using vscode I also added a config file for you to launch it in the debugger
import subprocess
import re

main_cmd = 'swipl -g runTest({nb_iterations},{ia1},{ia2}),halt -f testIAs.pl'


lines={}
lines[0]=' ;'

# Plus d'iteration = un resultat plus precis mais aussi un temps d'execution plus long
nb_iterations = 5
# IA par lequel on commence, regardez dans webserver pour avoir le nom des ia.
START_IA_ID = 2
# Choisir jusqu'a quelle ia on test
END_AI_ID = 5 # le max c'est 8, mais il ya un bug dans l'un d'entre eux, a vous de trouver lequel...

for i in range(START_IA_ID,END_AI_ID + 1):
	lines[0] = lines[0]+str(i)+';'
	lines[i-1] = str(i)+';'

for i in range(START_IA_ID,END_AI_ID + 1):
	for j in range(i,END_AI_ID):
		cmd = main_cmd.format(nb_iterations=nb_iterations,ia1=i,ia2=j)
		cmd_list = cmd.split()
		retour = subprocess.check_output(cmd_list).decode("utf-8")
		# print(retour)
		regex = re.search('^(.+) en commençant : a gagné (\d+) fois et a perdu (\d+).*\n(.+) en commençant : a gagné (\d+) fois et a perdu (\d+).*$', retour)
		
		# ia1 = regex.group(1)
		ia1_gagne_commencant = float(regex.group(2))
		ia1_perd_commencant = float(regex.group(3))
		
		# ia2 = regex.group(4)
		ia2_gagne_commencant = float(regex.group(5))
		ia2_perd_commencant = float(regex.group(6))
		if i-1 not in lines:
			lines[i-1] = ''
		lines[i-1] = lines[i-1] + str((ia1_gagne_commencant-ia1_perd_commencant)/nb_iterations) + ';'
		if(i!=j):
			if j-1 not in lines:
				lines[j-1] = ''
			lines[j-1] = lines[j-1] + str((ia2_gagne_commencant-ia2_perd_commencant)/nb_iterations) + ';'

print(lines)

with open('resultat_test.csv','w') as f:
	for indice in lines:
		f.write(lines[indice])
		f.write('\n')
