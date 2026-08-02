# Klimaanalyse Ruhrgebiet & Sauerland: Zeitliche und räumliche Temperaturunterschiede

Fallstudie aus dem Modul *Fallstudien I* (Fakultät Statistik, TU Dortmund).
Gruppenprojekt.

## Fragestellung

Hat sich das Klima im Ruhrgebiet über die letzten Jahrzehnte spürbar erwärmt?
Wie groß sind die Temperaturunterschiede zwischen sechs Stationen im
Ruhrgebiet und Sauerland – lassen sie sich allein durch die Höhenlage erklären?

**Daten:** Historische Tagesmitteltemperaturen (aggregiert zu Jahres-/Saisonwerten)
von sechs Wetterstationen (Duisburg, Essen, Dortmund, Arnsberg, Brilon, Kahler
Asten), bereitgestellt vom [European Climate Assessment & Dataset](https://www.ecad.eu/download/millennium/millennium.php).

## Vorgehen

| Schritt | Skript | Methode |
|---|---|---|
| Datenaufbereitung | `R/01_data_cleaning.R` | Import, Einheitenkonvertierung, fehlende Werte |
| Deskriptive Analyse | `R/02_eda.R` | Verteilungsform je Station (Boxplot, KDE, QQ-Plot) |
| Hypothesentests | `R/03_hypothesis_tests.R` | Welch-t-Test, Kruskal-Wallis, paarweiser Wilcoxon (Holm) |
| Regressionsanalyse | `R/04_adiabatic_effect.R` | Linearmodell, Modellvergleich |
| Räumliche Visualisierung | `R/05_spatial_map.R` | Stationskarte NRW (Temperatur & Höhe) |

## Ergebnisse

![Stationskarte](plots/station_map.png)

Auf einen Blick: Temperatur nimmt von Nordwesten (Duisburg, tief gelegen, wärmer)
Richtung Südosten (Kahler Asten, höchste Erhebung im Sauerland, deutlich kälter)
spürbar ab – ein erstes visuelles Indiz für den weiter unten quantifizierten
Höheneffekt.

**1. Zeitliche Erwärmung (Essen, 1950–1980 vs. 1990–2020)**

Die Jahresmitteltemperatur stieg von 9,52 °C auf 10,58 °C. Trotz der als statistisch homogen eingestuften Varianzen der beiden Zeiträume wurde ein
Welch-t-Test verwendet (geringer Power-Verlust gegenüber dem t-Test): **p < 0,001**, Effektstärke Cohen's d ≈ 1,6 (sehr großer
Effekt).

**2. Räumliche Unterschiede zwischen den 6 Stationen (1950–1995)**

Ein Kruskal-Wallis-Test (robuste Alternative zur ANOVA) zeigt statistisch
signifikante Unterschiede zwischen den Stationen (**p < 0,001**, ε² ≈ 0,84 –
starker Effekt). Der paarweise Wilcoxon-Test (Holm-korrigiert) zeigt, dass sich
alle Stationspaare signifikant unterscheiden – mit Ausnahme von Dortmund–Essen.

**3. Erklärt die Höhenlage die räumlichen Unterschiede?**

![Adiabatischer Effekt](plots/adiabatic_effect.png)

Eine lineare Regression der Jahresmitteltemperatur auf die Höhenlage bestätigt
den erwarteten adiabatischen Effekt fast exakt: **−0,65 °C pro 100 Höhenmeter**
(Referenzwert: −0,65 °C/100m). Ein Modellvergleich zeigt aber, dass ein Modell,
das zusätzlich die einzelne Station berücksichtigt, die Daten signifikant besser
erklärt (F-Test, p < 0,001) – die Höhenlage erklärt die räumlichen Unterschiede
also nur teilweise; weitere lokale Faktoren spielen ebenfalls eine Rolle.

## Statistische Methodik im Detail

- **Voraussetzungsprüfung vor Testwahl:** F-Test auf Varianzhomogenität vor
  Wahl zwischen Student- und Welch-t-Test
- **Robuste Alternativen:** Kruskal-Wallis statt ANOVA, da bei sechs Gruppen
  die Normalverteilungsannahme nicht überstrapaziert werden sollte
- **Korrektur für multiples Testen:** Holm-Korrektur bei 15 paarweisen
  Vergleichen, um die Inflation des Fehlers 1. Art zu kontrollieren
- **Effektstärken statt reiner p-Werte:** Cohen's d und Epsilon-Quadrat,
  um neben der Signifikanz auch die praktische Relevanz einzuordnen

## Reproduzierbarkeit

```bash
git clone https://github.com/kubaamarczak/case-studies-1
cd projekt-2
Rscript R/01_data_cleaning.R
Rscript R/02_eda.R
Rscript R/03_hypothesis_tests.R
Rscript R/04_adiabatic_effect.R
Rscript R/05_spatial_map.R
```

Benötigte Pakete: `dplyr`, `readr`, `tidyr`, `ggplot2`, `patchwork`, `sf`, `ggrepel`.

Die NRW-Geometrie für die Karte und der Ruhrverlauf benötigen Internetzugriff zur Ausführung.

## Tools

R (tidyverse, ggplot2)
