# =============================================================================
# Script:  gerar_qr_pix.R
# Autor:  Thiago Meireles
# Atualizado: 2026-11-19
#
# Entrada:
#   - Chave Pix, nome do recebedor e cidade (definidos abaixo)
# Saída:
#   - _data/pix.yml (código Pix "copia e cola" usado pelo site)
#   - assets/images/pix_qr.svg (QR code estático, sem valor fixo)
# =============================================================================

rm(list = ls()); gc()
options(scipen = 999)
cat("\014")

pacman::p_load(here, qrcode)

chave  <- "f00ec853-5259-4f9a-a17f-eea0c72b9743"
nome   <- "THIAGO DE O MEIRELES" # até 25 caracteres, sem acento
cidade <- "SAO PAULO"            # até 15 caracteres, sem acento

# Campo EMV: id de 2 dígitos + tamanho de 2 dígitos + valor
campo <- function(id, valor) {
  sprintf("%s%02d%s", id, nchar(valor, type = "bytes"), valor)
}

# CRC16-CCITT (polinômio 0x1021, valor inicial 0xFFFF), exigido pelo BR Code
crc16 <- function(texto) {
  passo_bit <- function(crc, i) {
    if (bitwAnd(crc, 0x8000) != 0) {
      bitwAnd(bitwXor(bitwShiftL(crc, 1L), 0x1021), 0xFFFF)
    } else {
      bitwAnd(bitwShiftL(crc, 1L), 0xFFFF)
    }
  }
  passo_byte <- function(crc, byte) {
    crc <- bitwXor(crc, bitwShiftL(byte, 8L))
    purrr::reduce(1:8, passo_bit, .init = crc)
  }
  crc <- purrr::reduce(as.integer(charToRaw(texto)), passo_byte, .init = 0xFFFF)
  sprintf("%04X", crc)
}

montar_br_code <- function(chave, nome, cidade) {
  stopifnot(nchar(nome) <= 25, nchar(cidade) <= 15)
  conta <- paste0(campo("00", "br.gov.bcb.pix"), campo("01", chave))
  corpo <- paste0(
    campo("00", "01"),            # versão do payload
    campo("26", conta),           # chave Pix
    campo("52", "0000"),          # categoria do comerciante
    campo("53", "986"),           # moeda: real
    campo("58", "BR"),
    campo("59", nome),
    campo("60", cidade),
    campo("62", campo("05", "***")), # sem identificador de transação
    "6304"
  )
  paste0(corpo, crc16(corpo))
}

codigo <- montar_br_code(chave, nome, cidade)

dir_imagens <- here::here("assets", "images")
if (!dir.exists(dir_imagens)) dir.create(dir_imagens, recursive = TRUE)

qrcode::generate_svg(
  qrcode::qr_code(codigo, ecl = "M"),
  filename = file.path(dir_imagens, "pix_qr.svg"),
  size = 240,
  show = FALSE
)

writeLines(
  c(
    "# Gerado por _app_scripts/pix/gerar_qr_pix.R (Thiago Meireles)",
    paste0("# Atualizado: ", Sys.Date()),
    paste0("copia_e_cola: \"", codigo, "\"")
  ),
  here::here("_data", "pix.yml")
)

cat(codigo, "\n")
