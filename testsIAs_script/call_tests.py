import subprocess
import re
from progress.bar import Bar


main_cmd = 'swipl -g runTest({nb_iterations},{ia1},{ia2}),halt -f testIAs.pl'

nb_iterations = 50

lines={}
lines[0]=' ;'
MIN = 2
MAX = 10
for i in range(MIN,MAX):
	lines[0] = lines[0]+str(i)+';'
	lines[i-1] = str(i)+';'
# bar = Bar('Processing', max=MAX*MAX)
# for i in range(MIN,MAX):
# 	for j in range(i,9):
# 		bar.next()
i = 3
j = 9
		# if(i < 9 and i > 6) or ((j < 9 and j > 6)):
		# 	continue;
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
# bar.finish()
print(lines)

with open('resultat_test.csv','w') as f:
	for indice in lines:
		f.write(lines[indice])
		f.write('\n')
