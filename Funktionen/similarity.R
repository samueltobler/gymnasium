# install.packages(c("textreuse", "readtext"))

library(textreuse)
library(readtext)

# --- Step 1: set your folder ---
folder <- "/Users/samuel/Downloads/HS25BerichteP"

# --- Step 2: read all PDF and DOCX files ---
# Important: use two separate patterns, then combine
texts_pdf  <- readtext(file.path(folder, "*.pdf"))
texts_docx <- readtext(file.path(folder, "*.docx"))

# Combine into one data frame
texts_df <- rbind(texts_pdf, texts_docx)

# Turn into a named list
texts <- as.list(texts_df$text)
names(texts) <- basename(texts_df$doc_id)

# --- Step 3: create a TextReuseCorpus ---
corpus <- TextReuseCorpus(
  texts = texts,
  tokenizer = tokenize_ngrams,
  n = 5,                               # adjust n for sensitivity
  minhash_func = minhash_generator(200)
)

# --- Step 4: compute pairwise Jaccard similarities ---
sim <- pairwise_compare(corpus, jaccard_similarity)

# --- Step 5: print and save results ---
print(sim)
write.csv(as.matrix(sim),
          file = "/Users/samuel/Downloads/similarities.csv")