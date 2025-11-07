# -*- coding: utf-8 -*-
"""
Script de Exemplo - Ampla
==========================

Este é um script de exemplo mostrando a estrutura básica recomendada.

Autor: Ampla
Data: 2025-11-07
"""

import pandas as pd
from pathlib import Path


def processar_dados(arquivo_entrada):
    """
    Processa dados de um arquivo.

    Args:
        arquivo_entrada (str): Caminho para o arquivo de entrada

    Returns:
        pd.DataFrame: Dados processados
    """
    # Lê dados
    df = pd.read_excel(arquivo_entrada)

    # Processa (exemplo)
    df_processado = df.copy()

    return df_processado


def main():
    """Função principal do script."""
    print("Script de exemplo executado com sucesso!")
    print("Adapte este template para suas necessidades.")


if __name__ == "__main__":
    main()
