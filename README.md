# Comparative Analysis of Concurrency in Java and Erlang.

### Abstract:
This thesis presents a comprehensive comparative analysis of concurrency models in Java and Erlang, two distinct programming languages with differing paradigms for handling concurrency. Java, an object-oriented language, employs the shared memory model, while Erlang, a functional language, utilizes the actor model. Through the measurement of latency, throughput, and resource usage, this study evaluates the performance of both languages in solving three distinct problems: CPU-bound, I/O-bound, and hybrid scenarios. By analyzing these metrics, the thesis provides insights into the strengths and weaknesses of each language and concurrency model. Furthermore, it considers the tool-sets and limitations provided by both languages in dealing with concurrency. This research contributes to a deeper understanding of the practical implications and trade-offs involved in selecting a concurrency model for real-world applications.
## Introduction:
As the technology of our computers advances in terms of computational power and the possibility of multi-core processors, the scale of problems and their complexity get greater. While these advancements make sequential programming faster and more efficient, it’s easy for developers to get comfortable even though there is great potential for utilizing the computational power that our hardware offers. The limitations that arise from the development of processors have caused multi-core processors to be more abundant and as our programs become more computationally heavy and handle larger amounts of data, the need for parallel programming becomes more important.

Parallel programming is a technique where the tasks are broken down into smaller subtasks that can be executed concurrently. This has emerged as a critical approach to harnessing the full potential of modern hardware architecture. By distributing these tasks across multiple cores, parallel programming allows for significant performance improvements and enables the efficient utilization of available resources. In recent years, parallel programming paradigms such as shared memory multiprocessing, distributed computing, and more, have gained a lot of traction, offering developers a diverse toolkit to handle increasingly complex computational challenges.

Transitioning from sequential programming to parallel programming however, has not been an easy task, as it poses several challenges. Coordinating execution of concurrent tasks, managing shared memory and ensuring thread safety are just a few of the obstacles that developers must navigate. Additionally, designing efficient parallel algorithms requires a deep understanding of both the underlying hardware architecture and the characteristics of the domain of the problem.
## Problem Statement:
The increasing demand for efficient handling of concurrent tasks in modern software applications requires a thorough understanding of concurrency, performance and resource utilization. However, the diverse range of languages offering concurrency presents developers with a challenging decision-making process when selecting the most suitable language for their applications. While there exists research on concurrency models between different languages, there lacks a comparative analysis that directly evaluates their performance and scalability in different types of scenarios.

## Purpose:
The purpose of this thesis is to compare Java and Erlang, and their ability to solve different types of workloads by using different metrics such as latency, throughput and resource usage. It aims to help developers decide on what language to use based on what type of workload they are facing. 

## Workload

- CPU bound - Matrix multiplication
- I/O bound - Webserver handling and distributing http requests
- Hybrid - Program that scrapes and encodes images from the web 

## Bibliography:
https://www.sciencedirect.com/science/article/pii/S0950584912001802

Mastering Concurrency in Python, Quan Nguyen

Java Concurrency in Practice, Brian Goetz

Programming Erlang: Software for a Concurrent World, Joe Armstrong

https://docs.oracle.com/javase/8/docs/api/

https://www.erlang.org/doc/

https://docs.python.org/3/

https://kth.diva-portal.org/smash/get/diva2:648395/FULLTEXT01.pdf

https://odr.chalmers.se/server/api/core/bitstreams/d1234ca1-eb72-4c9d-b403-fbb52d32f4e9/content

https://www.e-reading.club/bookreader.php/139986/Programming_Erlang.pdf

https://ieeexplore.ieee.org/document/4476447/similar#similar

https://dl.acm.org/doi/pdf/10.1145/1095408.1095421




