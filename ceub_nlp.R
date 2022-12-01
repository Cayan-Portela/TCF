library(lsa)
library(tidytext)
library(tidyverse)
library(janeaustenr)

noticia_1 <- c("Dois dos melhores jogadores das últimas décadas correm o risco de se despedir definitivamente da Copa do Mundo nesta quarta-feira. Às 16h (de Brasília), a Polônia, de Robert Lewandowski, e a Argentina, de Lionel Messi, se enfrentam no estádio 974 pela última rodada do Grupo C.")
noticia_2 <- c("A modelo Jessica Turini, apontada em agosto deste ano como o novo affair de Neymar, quando ele terminou o relacionamento com Bruna Biancardi, está no Catar. Com a repercussão do suposto relacionamento entre a influencer e o camisa 10, os internautas repararam na semelhança da modelo de 30 anos com Bruna Marquezine, ex-namorada do craque brasileiro e seu relacionamento mais duradouro, conhecido e assumido.")
noticia_3 <- c("A CBF atualizou nesta terça-feira o boletim médico do lateral-direito Danilo e do atacante Neymar. Em comunicado, o chefe do departamento médico brasileiro, Rodrigo Lasmar, disse que os dois continuam em processo de recuperação das respectivas lesões de tornozelo e que não vão enfrentar Camarões na sexta-feira.")
noticia_4 <- c("Dos 26 jogadores convocados pelo técnico Tite para defender a Seleção na Copa do Mundo, sete aguardam ansiosamente pela oportunidade de estrear no Catar. Depois de assistirem aos duelos contra Sérvia e Suíça do banco de reservas, eles podem receber uma chance de ouro na sexta-feira, quando o Brasil encara Camarões, às 16h (de Brasília), pela última rodada da fase de grupos.")

t1 <- tibble(
    noticia = c("noticia_1", "noticia_2", "noticia_3", "noticia_4"),
    txt = c(noticia_1, noticia_2, noticia_3, noticia_4)
    )

noticia_words <- t1 %>%
    unnest_tokens(word, txt) %>%
    count(noticia, word, sort=TRUE)

total_words <- noticia_words %>%
    group_by(noticia) %>%
    mutate(total = sum(n)) %>%
    ungroup()

noticia_tf_idf <- total_words %>%
  bind_tf_idf(word, noticia, n)


final <- tibble(word = unique(total_words$word))

word_1 <- tibble(noticia = "noticia_1", txt = noticia_1)
word_2 <- tibble(noticia = "noticia_2", txt = noticia_2)
word_3 <- tibble(noticia = "noticia_3", txt = noticia_3)
word_4 <- tibble(noticia = "noticia_4", txt = noticia_4)

word_1_freq <- word_1 %>%
    unnest_tokens(word, txt) %>%
    left_join(
        noticia_tf_idf,
        by = c("noticia", "word")
    ) %>%
    rename(tf_idf_1 = tf_idf) %>%
    select(word, tf_idf_1)

word_2_freq <- word_2 %>%
    unnest_tokens(word, txt) %>%
    left_join(
        noticia_tf_idf,
        by = c("noticia", "word")
    ) %>%
    rename(tf_idf_2 = tf_idf) %>%
    select(word, tf_idf_2)

word_3_freq <- word_3 %>%
    unnest_tokens(word, txt) %>%
    left_join(
        noticia_tf_idf,
        by = c("noticia", "word")
    ) %>%
    rename(tf_idf_3 = tf_idf) %>%
    select(word, tf_idf_3)

word_4_freq <- word_4 %>%
    unnest_tokens(word, txt) %>%
    left_join(
        noticia_tf_idf,
        by = c("noticia", "word")
    ) %>%
    rename(tf_idf_4 = tf_idf) %>%
    select(word, tf_idf_4)

df_final <- final %>%
    left_join(word_1_freq, by = "word") %>%
    left_join(word_2_freq, by = "word") %>%
    left_join(word_3_freq, by = "word") %>%
    left_join(word_4_freq, by = "word") %>%
    unique()

df_final[ is.na(df_final) ] <- 0

matriz <- cbind(
    df_final$tf_idf_1,
    df_final$tf_idf_2,
    df_final$tf_idf_3,
    df_final$tf_idf_4
    )

head(noticia_1)
head(noticia_2)
head(noticia_3)
head(noticia_4)

cosine(matriz)



# Exercicio 1
ab1 <- c("Brasil está classificado para as oitavas de final")
ab2 <- c("Argentina briga por uma vaga nas oitavas de final")

final2 <- tibble(
    word = str_split(c(ab1, ab2), pattern = " ") %>%
     unlist())

final2 <- final2 %>%
    mutate(
        TF_1 = c(rep(1/8, 8), rep(0, 6), rep(0, 3)),
        TF_2 = c(rep(0, 8), rep(1/9, 9))
        ) %>%
    mutate(
        IDF = c(rep(log(2/1), 5),
                rep(log(2/2), 3),
                rep(log(2/1), 6),
                rep(log(2/2), 3))
    ) %>%
    mutate(
        TF_IDF_1 = TF_1 * IDF,
        TF_IDF_2 = TF_2 * IDF
    )

cosine(final2$TF_IDF_1, final2$TF_IDF_2)


t_ab <- tibble(
    noticia = c("noticia_1", "noticia_2"),
    txt = c(ab1, ab2)
    )
