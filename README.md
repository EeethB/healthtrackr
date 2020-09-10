# Summary

Project to turn my feature phone into a personal health tracker. Used to track progress toward general wellness goals, including but not limited to: Fruit, veggie, and water consumption, exercise, connection with family, and overall wellness. Currently just a couple scripts that update a history table, but it has aspirations of package status.

# Motivations

I was originally tracking these things with pencil and paper, but I ran into a few problems, like losing track of the papers or not being able to easily analyze the data over time. In order to solve these problems, I wanted to design a process that:

  - Was SMS based. I almost always have my phone with me, so I can record things in real time. It's also not a smart phone, so I couldn't just get some app.
  - Tracked results over time so I could see how I was doing on my goals.

# Data flow

I knew that I could send SMS to a Gmail account, so I set up a dedicated account for wellness tracking. I start by texting my Gmail account some information about wellness, such as "Went climbing for 35 minutes." Then I pull down all those messages using `gmailr`, map them to columns in my wellness table (`ex_climb`), and extract the value included.

# Goals

  - Create more expansive analysis
  - Turn into a package
    - Which functions to expose?
    - How to add new columns?
      - Will require knowledge of regex for users
      - Maybe mix in GAB's RegExplain
    - How much of the data storage and authentications should be handled automatically?
