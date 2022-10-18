library(tidyverse)

lyric_df <- read.csv("/Users/noahjohnson/Downloads/bluefall2hwteam17/sent_emo_lyrics.csv")

lyric_df_comp <- lyric_df %>% arrange(desc(Compound.Sentiment)) %>% head(20)
lyric_df_pos_sent <- lyric_df %>% arrange(desc(Positive.Sentiment)) %>% head(20)
lyric_df_neg_sent <- lyric_df %>% arrange(desc(Negative.Sentiment)) %>% head(20)

lyric_df_foreign <- lyric_df %>% arrange(desc(num_fw)) %>% head(20)

lyric_df_anger <- lyric_df %>% arrange(desc(Anger)) %>% head(20)
lyric_df_antic <- lyric_df %>% arrange(desc(Anticipation)) %>% head(20)
lyric_df_disg <- lyric_df %>% arrange(desc(Disgust)) %>% head(20)
lyric_df_fear <- lyric_df %>% arrange(desc(Fear)) %>% head(20)
lyric_df_joy <- lyric_df %>% arrange(desc(Joy)) %>% head(20)
lyric_df_neg <- lyric_df %>% arrange(desc(Negative)) %>% head(20)
lyric_df_pos <- lyric_df %>% arrange(desc(Positive)) %>% head(20)
lyric_df_sad <- lyric_df %>% arrange(desc(Sadness)) %>% head(20)
lyric_df_surp <- lyric_df %>% arrange(desc(Surprise)) %>% head(20)
lyric_df_trust <- lyric_df %>% arrange(desc(Trust)) %>% head(20)

lyric_df <- lyric_df %>% mutate(Expanded.Valence = round(-Anger + Anticipation - Disgust - Fear + Joy - Negative + Positive - Sadness + Surprise + Trust, 5))
lyric_df <- lyric_df %>% dplyr::select(-Exp)
write.csv(lyric_df, "/Users/noahjohnson/Downloads/bluefall2hwteam17/sent_emo_lyrics.csv")

sent_metrics <- read.csv("/Users/noahjohnson/Downloads/bluefall2hwteam17/yearly_sent_metrics.csv")

