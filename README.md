This is the repository for our work “*Tennis players exploit prior information to improve performance: evidence for continuous anticipatory decision-making*” containing data and code used for the data analysis, the experimental protocols, c3d files of a 21 skeleton model of each return of each tennis player, and visualisation.

We developed an immersive VR tennis environment, where we tested how tennis players exploit prior knowledge to improve performance und continuous decision-making when returning tennis serves.

<img src="./experimental_task/experimental_task.gif" alt="image" width="700" height="auto">

[Watch the full video on SWITCHtube](https://tube.switch.ch/videos/2otCdMkJpF)

## Performance development according to integrating prior knowledge about serve probabilities in the congruent and incongruent cases
Development of correct response rate and hit rate over all blocks with a probability of 20% in the incongruent case to one side and 80% in the congruent case to the other side. While players increased performance in the congruent played serves, they decreased in the incongruent played serves.
<table style="table-layout: fixed; width: 100%;">
  <tr>
    <th style="width: 50%;">Correct response rates</th>
    <th style="width: 50%;">Hit rate</th>
  </tr>
  <tr>
    <td style="text-align: center;">
      <img src="./plots/sqrt_correct_response_rate.svg" alt="Correct response rates" style="width:350px;height:auto;">
    </td>
    <td style="text-align: center;">
      <img src="./plots/sqrt_hit_rate.svg" alt="hit rate" style="width:350px;height:auto;">
    </td>
  </tr>
</table>

<p>Figure 1 Development of tennis player’s correct response rate (left) to go the correct side and hit rate (right) during the experiment. The congruent trials were played with an 80% chance and the incongruent with a 20% chance. The trials before were played with a 50% chance on both sides, such as they started with a neutral prior. We took the root of the trial number as the regression predictor according the exponential law of practice (Heathcote & Brown, 2000). The uncertainty area represents a 95% confidence interval of the regression lines.</p>
## Weight shift over a return
<img src="./plots/prior_impact_over_second_half_of_biased_and_neutral_trials_on_weight_shift.png" style="width:850px;height:auto;">

Figure 2 Development of tennis player’s weight shifting in returning a tennis serve during the experiment. The congruent trials were played with an 80% chance and the incongruent with a 20% chance. The trials before were played with a 50% chance on both sides, such as they started with a neutral prior. A positive value means a weight shift directed to the more probable side (the prior).

## Repository structure 

The folders [c3d_files](./c3d_files) contain from every trial of each tennis player a c3d file with a 21 point full body model and the mokka software to visualise the c3d files. The R script is in the folder [code](./code), all the data used for the analysis is stored in a csv file in the folder [data](./data), each individual experimental protocol can be find in [experimental_protocols](./experimental_protocols), a video of the experimental task can be watched on [Watch the full video on SWITCHtube](https://tube.switch.ch/videos/2otCdMkJpF) or an excerpt in the folder [experimental_task](./experimental_task), all the plots are in [plots](./plots), and there is the R studio project to reproduce the calculations of all statistical analyses and plots production. The experimental_protocols comes in separate spread sheet files (in xlsx format), which are structured as follows:



|       Column name       | Description                        |
|--------------|--------------------------|
| session      | name of the session       |
| block        | block ID                  |
| trial        | trial number              |
| left_right   | side played (left/right)  |

The data file all_data in [data](./data) is structured as follows:
| Column name                             | Description  |
|------------------------------------------|-------|
| vp                                       |  player     |
| trial                                    |    trial number   |
| condition                                |   warm up/neutral/congruent/incongruent    |
| side_played                              |    left/right   |
| correct_response                         |   1 = true, 0 = false    |
| hit_true_false                           |    1 = true, 0 = false   |
| serve_time                               |   time in relation to the serve    |
| bounce_time                              |   time in relation to the serve    |
| hit_time                                 |   time in relation to the serve    |
| splitstep_performed_com_5cm_below_max     |   1 = true, 0 = false    |
| splitstep_start                          |   time in relation to the serve    |
| lateral_movement_initiation               |   time in relation to the serve    |
| side_tendency_minus_200                  |   weight shift (positive means to the left)    |
| side_tendency_minus_195                  |   weight shift (positive means to the left)    |
| side_tendency_minus_190                  |   weight shift (positive means to the left)    |
| .                  |   .    |
| .                  |   .    |
| .                  |   .    |
| side_tendency_minus_005                  |   weight shift (positive means to the left)    |
| side_tendency_000                        |   weight shift (positive means to the left)    |
| side_tendency_005                        |   weight shift (positive means to the left)    |
| .                        |   .    |
| .                        |   .    |
| .                        |   .    |
| side_tendency_990                        |    weight shift (positive means to the left)   |
| side_tendency_995                        |    weight shift (positive means to the left)   |
| side_tendency_100                        |   weight shift (positive means to the left)    |

