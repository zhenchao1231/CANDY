# CANDY
Conversation ANalysis of DYnamics

A tool designed to extract interpretable macro-level features from conversation, based on the methodology described by Di Stasi and colleagues (2024).

## Components

A .R file for packaged functions, ready to use with instructions and assumptions below.  
A .Rmd notebook file detailing the calculation of each metric for references and customization.

## Assumptions

Input data.frame has at least: speaker, start, stop (row = turn).  

Optional columns:  
- n_words   (for speech-rate metrics; see helper in .R to compute from text)  
- overlap   (logical / 0-1 / "True"/"False"); see helper in .R to add if missing  

## Output

  Each metric function returns a tibble with one row per `speaker`.  
  A convenience wrapper `cd_all_metrics()` returns a named list of results.

## Usage
```
  source("ConvoDynamics.R")  
  res <- cd_all_metrics(audio)

  res$speaking_time; res$turn_length;   
  res$speech_rate; res$backchannel; res$response_time
```

## Features

The package currently supports deriving the following conversation-level, speaker-specific metrics:

- **Speaking Time**: Percentage of total conversation time each speaker talks.
- **Turn Length**: Statistical measures of speaking turn durations.
- **Speech Rate**: Speed at which a negotiator talks in words per  minute (WPM).
- **Backchannels**: Instances of sub-1-s utterances during the  counterpart’s turn.
- **Response Time**: Duration of silence between the end of the  counterpart’s turn and the first voiced  utterance from the negotiator.

For speaking time and backchannels, we calculate the central tendency for each speaker. For turn length, speech rate, and response time, we calculate four measures for each conversation dynamics metric: median, coefficient of variation, adaptability, and predictability.

## Contributing

Contributions are welcome! Please open issues or submit pull requests for improvements or bug fixes.

## License

This project is licensed under the MIT License.

## References

Di Stasi, M., Templeton, E., & Quoidbach, J. (2024). Zooming out on bargaining tables: Exploring which conversation dynamics predict negotiation outcomes. *Journal of Applied Psychology*, *109*(7), 1077–1093. https://doi.org/10.1037/apl0001136
