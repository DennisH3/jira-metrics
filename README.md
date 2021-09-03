[(Français)](#le-nom-du-projet)

# Jira Metrics

jiraR is an R Package that uses exported Jira Issues to create data visualizations to report metrics defined by the user. It includes an RMarkdown solution as well as an RShiny GUI solution.

There are 4 main files:
- countIssues.R: contains functions that count issues based on closed epics, new issues, active issues, division, and workspace. It also includes a generalized count issues function.
- plotMetrics.R: creates the visualizations for the respective function in countIssues.R
- xmltodf.R: converts exported Jira Issues XML file to a data frame object
- metricsApp.R: the GUI for displaying the data visualizations of the metrics. Requires the Jira Issues to be input as CSV or XML.

For an example of the jiraR package being used, refer to [/vignettes/jiraMetrics(Direct).Rmd](https://github.com/DennisH3/jira-metrics/blob/master/vignettes/jiraMetrics(Direct).Rmd)

The example, directly pulls Jira issues into R using the Jira API instead of loading from a CSV or XML file.

### How to Contribute

See [CONTRIBUTING.md](CONTRIBUTING.md)

### License

Unless otherwise noted, the source code of this project is covered under Crown Copyright, Government of Canada, and is distributed under the [MIT License](LICENSE).

The Canada wordmark and related graphics associated with this distribution are protected under trademark law and copyright law. No permission is granted to use them outside the parameters of the Government of Canada's corporate identity program. For more information, see [Federal identity requirements](https://www.canada.ca/en/treasury-board-secretariat/topics/government-communications/federal-identity-requirements.html).

______________________

## Le nom du projet

- Quel est ce projet?
- Comment ça marche?
- Qui utilisera ce projet?
- Quel est le but de ce projet?

### Comment contribuer

Voir [CONTRIBUTING.md](CONTRIBUTING.md)

### Licence

Sauf indication contraire, le code source de ce projet est protégé par le droit d'auteur de la Couronne du gouvernement du Canada et distribué sous la [licence MIT](LICENSE).

Le mot-symbole « Canada » et les éléments graphiques connexes liés à cette distribution sont protégés en vertu des lois portant sur les marques de commerce et le droit d'auteur. Aucune autorisation n'est accordée pour leur utilisation à l'extérieur des paramètres du programme de coordination de l'image de marque du gouvernement du Canada. Pour obtenir davantage de renseignements à ce sujet, veuillez consulter les [Exigences pour l'image de marque](https://www.canada.ca/fr/secretariat-conseil-tresor/sujets/communications-gouvernementales/exigences-image-marque.html).
