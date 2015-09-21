# Continuous Delivery Visualizations and Metrics for Radiator

We implemented a set of data mining tools and visualizations to support evaluation of continuous delivery in a project. The work was done in [Need 4 Speed program](http://n4s.fi) in co-operation with Solita and Tampere University of Technology.

See publication [Mashing up software issue management, development, and usage data](http://dl.acm.org/citation.cfm?id=2820685) for details. Abstract:  Modern software development approaches rely extensively on tools. Motivated by practices such as continuous integration, deployment and delivery, these tools are used in a fashion where data are automatically accumulated in different databases as a side-effect of everyday development activities. In this paper we introduce an approach for software engineering data visualization as a mashup that combines data from issue management, software development and production use. The visualization can show to all stake holders how well continuous delivery is realized in the project. The visualization clearly shows the time spent to specify and develop the features as well the length of the delivery cycle. Further more the visualization shows how much work is unfinished and waiting for delivery. This can help the development team to decrease the amount of unfinished work and by that help them to keep up in continuous delivery mind set. In addition to development data usage of the features is also visualized.

![Status](/img/status.png)

In addition to this, we implemented a Dashing plugin for showing the information in the team workspace:

![Status](/img/fig-workspace.jpg)

The radiator shows the age of oldest done feature that has not been deployed to the production environment. For example, if a feature was implemented three days ago, but has not been deployed to the production environment:

![Oldest Done Feature Ok](/img/odfpe.png)

When the oldest feature is one week old, the radiator turns to red:

![Oldest Done Feature Fail](/img/odfpe2.png)

This may help the team to achieve continuous delivery. The metric for case project [Lupapiste](http://lupapiste.fi) during a one year period show the following information:

![1 year](/img/fig-one-year.png)

The team has been delivering continuously, but the one week limit has been broken for more than 10 times.
