# Comparative Analysis of Concurrency in Java and Erlang.

### Abstract
This thesis presents a comprehensive comparative analysis of concurrency models in Java and Erlang, two distinct programming languages with differing paradigms for handling concurrency. Java, an object-oriented language, employs the shared memory model, while Erlang, a functional language, utilizes the actor model. Through the measurement of latency, throughput, and resource usage, this study evaluates the performance of both languages in solving three distinct problems: CPU-bound, I/O-bound, and hybrid scenarios. By analyzing these metrics, the thesis provides insights into the strengths and weaknesses of each language and concurrency model. Furthermore, it considers the tool-sets and limitations provided by both languages in dealing with concurrency. This research contributes to a deeper understanding of the practical implications and trade-offs involved in selecting a concurrency model for real-world applications.
## Introduction
As the technology of our computers advances in terms of computational power and the possibility of multi-core processors, the scale of problems and their complexity get greater. While these advancements make sequential programming faster and more efficient, it’s easy for developers to get comfortable even though there is great potential for utilizing the computational power that our hardware offers. The limitations that arise from the development of processors have caused multi-core processors to be more abundant and as our programs become more computationally heavy and handle larger amounts of data, the need for parallel programming becomes more important.

Parallel programming is a technique where the tasks are broken down into smaller subtasks that can be executed concurrently. This has emerged as a critical approach to harnessing the full potential of modern hardware architecture. By distributing these tasks across multiple cores, parallel programming allows for significant performance improvements and enables the efficient utilization of available resources. In recent years, parallel programming paradigms such as shared memory multiprocessing, distributed computing, and more, have gained a lot of traction, offering developers a diverse toolkit to handle increasingly complex computational challenges.

Transitioning from sequential programming to parallel programming however, has not been an easy task, as it poses several challenges. Coordinating execution of concurrent tasks, managing shared memory and ensuring thread safety are just a few of the obstacles that developers must navigate. Additionally, designing efficient parallel algorithms requires a deep understanding of both the underlying hardware architecture and the characteristics of the domain of the problem.

## Background
### Concurrency In Programming
In the beginning of computers, there didn’t exist any operating systems. Their only purpose was to execute a single program from start to end, that program had access to all the system resources that were available in the machine. Writing these programs was quite hard, but most of all, it was an inefficient and expensive use of scarce computer resources. As technology evolved, operating systems were introduced which allowed more than one program to run at once on a machine in individual processes. These were isolated, independently executing programs which were allocated resources by the system such as memory, file handles and security credentials. These processes were also able to communicate with each other through a variety of different communication mechanisms such as sockets, signal handles, shared memory, semaphores and files. Factors such as resource utilization, fairness and convenience led to the development of these operating systems which allowed for the possibility of executing multiple programs simultaneously.

Programs during this time were written in what is called the sequential programming model. This was the most intuitive and natural way of writing code as it best described human behavior, doing one thing at a time, in a sequence. As technology continued evolving, the need for running processes simultaneously and asynchronously grew which motivated the development of what we know as threads. A lightweight process that allows multiple streams of program control flow to exist within the same process. The threads share the same resources that are allocated to the process but provide a way to exploit parallelism on multiprocessor systems.
### Java And The Shared Memory Model
### Erlang and The Actor Model
The actor model was created back in 1973 by Carl Hewitt. It was created with concurrency in mind where everything in a system is considered an actor. The actors within the system handle state by communicating changes or actions to be taken through messages. 

The Erlang language was originally created and developed at Ericsson in the late 1980’s. It was created out of a need for a concurrent, fast and fault-tolerant language to manage their telephone Hswitches. Since there was no language that suited that need available, Ericsson decided on creating their own language. Erlang is a functional language which does not share state, state is instead handled by using the actor model mentioned above. 

By embracing functional programming principles, with immutable data and using the actor model, Erlang is able to remove the need for locks and synchronization. This makes the language inherently robust and scalable.

At the early development of the language, the prolog interpreter was used to run the code but since that was deemed too slow, they created the BEAM virtual machine. By compiling the code to C and running it on the BEAM they were able to improve the runtime significantly. Today, the BEAM is more than just a runtime environment for Erlang; it serves as the foundation for languages like Elixir and Gleam, which leverage its capabilities for compilation and execution. 
## Problem Statement
The increasing demand for efficient handling of concurrent tasks in modern software applications requires a thorough understanding of concurrency, performance and resource utilization. However, the diverse range of languages offering concurrency presents developers with a challenging decision-making process when selecting the most suitable language for their applications. While there exists research on concurrency models between different languages, there lacks a comparative analysis that directly evaluates their performance and scalability in different types of scenarios.

## Purpose
The purpose of this thesis is to compare Java and Erlang, and their ability to solve different types of workloads by using different metrics such as latency, throughput and resource usage. It aims to help developers decide on what language to use based on what type of workload they are facing. 
## Research Question
How do the concurrency models of Java and Erlang, represented by the shared memory model and actor model respectively, perform in terms of latency, throughput, and resource usage across CPU-bound, I/O-bound, and hybrid workloads?
## Workload

- CPU bound - Matrix multiplication
- I/O bound - Webserver handling and distributing http requests
- Hybrid - Program that scrapes and encodes images from the web 

## Bibliography
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




