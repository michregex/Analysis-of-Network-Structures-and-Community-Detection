
# Stack Exchange Tag Network Analysis

This project provides a comprehensive analysis of the network structure and relationships of tags used on [Stack Exchange’s Statistics community](https://stats.stackexchange.com/), utilizing network analysis techniques. The project explores the importance of tags in content organization and information retrieval within the community.

## Project Overview

The goal of this project is to examine the relationships between tags applied to questions on Stack Exchange, specifically focusing on the co-tagging of topics. By using network analysis methods, this project explores the statistical properties of the tag network, evaluates different centrality and clustering measures, and applies community detection algorithms.

## Table of Contents

- [Data Collection](#data-collection)
- [Exploratory Data Analysis](#exploratory-data-analysis)
- [Centrality Measures Comparison](#centrality-measures-comparison)
- [Degree Distribution and Power-law](#degree-distribution-and-power-law)
- [Community Detection](#community-detection)
- [Results and Comparison](#results-and-comparison)
- [Conclusions](#conclusions)
- [References](#references)

## Data Collection

Data was collected from the Statistics Stack Exchange community using Stack Exchange's API, yielding 3,000 questions. Each question includes tags, which were analyzed to construct an edge table. Edges represent co-tagging events between pairs of tags, with weights reflecting their frequency. After filtering, the network comprised 1,000 nodes and 8,447 edges.

## Exploratory Data Analysis

### Global Measures

We computed several key statistics, including:
- **Mean Degree**: 16.89
- **Mean Weighted Degree**: 29.002
- **Global Clustering Coefficient**: 0.164
- **Graph Density**: 0.017

These metrics provide insight into the network's sparse, weakly clustered structure.

## Centrality Measures Comparison

In addition to degree centrality, eigenvector centrality was computed to assess node importance. Tags related to "regression," "machine-learning," and "R" showed the highest centralities, highlighting their significance in the community.

## Degree Distribution and Power-law

The network's degree distribution follows a power-law, indicating a complex structure where a small number of highly connected nodes exist alongside many less connected nodes.

## Community Detection

Community detection was performed using modularity maximization and a simulated annealing optimization algorithm, resulting in five communities. The modularity score achieved was 0.369, indicating the presence of some community structure.

### Simulated Annealing Optimization

The algorithm implemented in R optimizes community modularity through simulated annealing. The modularity of a community is calculated by iteratively splitting and merging communities until optimal partitions are reached. This method efficiently balances precision with computational feasibility.

## Results and Comparison

The partition obtained through our simulated annealing algorithm was compared to results from Gephi’s Louvain method. Our algorithm produced a modularity score of 0.369 with five communities, closely aligned with Gephi’s score of 0.374, which detected seven communities. Evaluation using Mutual Information (MI = 0.887) and Rand Index (RI = 0.856) further confirmed a strong similarity between the partitions.

## Conclusions

The analysis successfully identifies key network characteristics and demonstrates that tags are a vital component in organizing content. Despite some differences in detected communities, the results were visually and quantitatively similar, suggesting that our algorithm provides a reliable alternative to established methods.

## References

1. Fu X, Yu S, Benson AR (2019) Modeling and analysis of tagging networks in Stack Exchange communities.
2. Bastian M, Heymann S, Jacomy M (2009) Gephi: An open source software for exploring and manipulating networks.
3. Newman M (2010) Networks: An Introduction. Oxford University Press, Inc.

---

## How to Run the Project

1. **Data Collection**: Use the Stack Exchange API to collect data as described.
2. **Network Analysis**: Run the R script `modularityOptimization.R` to compute community structures and centrality metrics.
3. **Visualization**: Use Gephi for network visualization and to verify the modularity-based community structure.

