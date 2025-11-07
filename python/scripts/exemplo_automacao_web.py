# -*- coding: utf-8 -*-
"""
Automação Web - Exemplo com Playwright
=======================================

Script de exemplo mostrando uso básico do Playwright para automação web.

Autor: Ampla
Data: 2025-11-07
"""

from playwright.sync_api import sync_playwright
import time


def exemplo_navegacao_basica():
    """
    Exemplo básico de navegação com Playwright.

    Abre um navegador, navega para uma página e captura screenshot.
    """
    with sync_playwright() as p:
        # Abre navegador (chromium, firefox ou webkit)
        browser = p.chromium.launch(headless=False)  # headless=True para modo invisível
        page = browser.new_page()

        # Navega para URL
        page.goto("https://www.example.com")

        # Aguarda elemento carregar
        page.wait_for_load_state("networkidle")

        # Captura screenshot
        page.screenshot(path="screenshot.png")

        print("Screenshot capturado com sucesso!")

        # Fecha navegador
        browser.close()


def exemplo_preenchimento_formulario():
    """
    Exemplo de preenchimento de formulário.
    """
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=False)
        page = browser.new_page()

        page.goto("https://www.example.com/formulario")

        # Preenche campos
        page.fill("#campo_nome", "Texto de exemplo")
        page.fill("#campo_email", "exemplo@ampla.com")

        # Clica em botão
        page.click("button[type='submit']")

        # Aguarda resposta
        page.wait_for_selector(".mensagem-sucesso")

        browser.close()


def exemplo_extracao_dados():
    """
    Exemplo de extração de dados de uma página.
    """
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page()

        page.goto("https://www.example.com")

        # Extrai texto de elemento
        titulo = page.text_content("h1")
        print(f"Título: {titulo}")

        # Extrai múltiplos elementos
        links = page.query_selector_all("a")
        for link in links:
            href = link.get_attribute("href")
            texto = link.text_content()
            print(f"Link: {texto} -> {href}")

        browser.close()


def main():
    """Função principal do script."""
    print("Exemplos de automação web com Playwright")
    print("-" * 50)

    # Descomente o exemplo que deseja executar:
    # exemplo_navegacao_basica()
    # exemplo_preenchimento_formulario()
    # exemplo_extracao_dados()

    print("\nAdapte estes exemplos para suas necessidades!")


if __name__ == "__main__":
    main()
