# -*- coding: utf-8 -*-
"""
Automação Informakon - Login e Navegação
=========================================

Script para automatizar login e navegação no sistema Informakon.

Autor: Ampla
Data: 2025-11-07
"""

import asyncio
import re
from playwright.async_api import async_playwright

# ✅ Caminho do arquivo com credenciais
CAMINHO_CREDENCIAIS = r"C:\Users\Ampla\OneDrive - AMPLA INCORPORADORA LTDA\Documentos\Chaves.txt"

def obter_credenciais():
    """Lê o arquivo de credenciais e retorna email e senha."""
    try:
        with open(CAMINHO_CREDENCIAIS, "r", encoding="utf-8") as arquivo:
            conteudo = arquivo.read()

            # Expressões regulares para capturar email e senha dentro de aspas
            email_match = re.search(r'konstroi_login:\s*"([^"]+)"', conteudo)
            senha_match = re.search(r'konstroi_senha:\s*"([^"]+)"', conteudo)

            if email_match and senha_match:
                return email_match.group(1), senha_match.group(1)
            else:
                raise ValueError("⚠️ Erro ao encontrar email ou senha no arquivo.")
    except Exception as e:
        print(f"❌ Erro ao ler credenciais: {e}")
        return None, None

async def fazer_login():
    """Abre o site, faz login, lida com notificações, clica em 'Abrir no Navegador'
    e, por fim, tenta clicar em 'Informakon' - lidando com a possibilidade de abrir nova página."""

    # 1. Obter credenciais
    email, senha = obter_credenciais()
    if not email or not senha:
        print("❌ Credenciais não encontradas. Encerrando o script.")
        return

    async with async_playwright() as playwright:
        # 2. Iniciar o navegador
        navegador = await playwright.chromium.launch(
            headless=False,
            args=["--disable-blink-features=AutomationControlled"]
        )
        contexto = await navegador.new_context()
        pagina = await contexto.new_page()

        # 3. Acessar a página de login e aguardar carregamento completo
        url = "https://konstroi.autosky.cloud/"
        print(f"🌐 Acessando: {url}")
        await pagina.goto(url)
        await pagina.wait_for_load_state("networkidle")

        # 4. Preencher campos de email e senha
        try:
            campo_email = await pagina.wait_for_selector("input[name='email'], input[id='login'], input[type='text']")
            await campo_email.fill(email)
            print("✅ Email preenchido.")

            campo_senha = await pagina.wait_for_selector("input[name='senha'], input[id='senha'], input[type='password']")
            await campo_senha.fill(senha)
            print("✅ Senha preenchida.")

            # 5. Clicar no botão de login
            botao_entrar = await pagina.wait_for_selector("button[type='submit'], button:has-text('Entrar')")
            await botao_entrar.click()
            print("🚀 Login enviado.")

            # 6. Esperar carregamento completo da página pós-login
            await pagina.wait_for_load_state("networkidle")
            print("🎯 Login concluído com sucesso.")
        except Exception as e:
            print(f"❌ Erro durante o login: {e}")
            return

        # 7. Se houver aviso de outro dispositivo, clicar em "Continuar"
        try:
            botao_continuar = await pagina.wait_for_selector("button:has-text('Continuar'), a:has-text('Continuar')", timeout=15000)
            await botao_continuar.click()
            print("✅ Clicou em 'Continuar'.")
            await pagina.wait_for_load_state("networkidle")
            print("✅ Página carregada após clicar em 'Continuar'.")
        except Exception as e:
            print(f"⚠️ Aviso de outro dispositivo não detectado ou erro ao clicar em 'Continuar': {e}")

        # 8. Clicar na opção 'Abrir no Navegador'
        try:
            # Listen for a new page event (some systems open a new tab)
            wait_for_new_page = contexto.wait_for_event("page", timeout=60000)

            botao_navegador = await pagina.wait_for_selector("button:has-text('Abrir no Navegador')", timeout=60000)
            await botao_navegador.click()
            print("✅ Clicou em 'Abrir no Navegador'.")

            # Se uma nova página abrir, capturamos aqui
            try:
                nova_pagina = await wait_for_new_page
                print("🔄 Nova página/tab detectada para 'Abrir no Navegador'.")
            except asyncio.TimeoutError:
                nova_pagina = None
                print("⚠️ Nenhuma nova página detectada dentro de 60s; Continuando na página atual.")

        except Exception as e:
            print(f"❌ Erro ao encontrar o botão 'Abrir no Navegador': {e}")
            nova_pagina = None

        # 9. Se abriu uma nova página, trabalhamos nela; caso contrário, usamos 'pagina'
        if nova_pagina is not None:
            await nova_pagina.wait_for_load_state("networkidle")
            print("✅ Nova página carregada após clicar em 'Abrir no Navegador'.")
            final_page = nova_pagina
        else:
            await pagina.wait_for_load_state("networkidle")
            print("✅ Página final carregada (sem nova aba).")
            final_page = pagina

        # 10. Tentar localizar e clicar em "Informakon" na página (ou nova_pagina)
        try:
            botao_informakon = await final_page.wait_for_selector("button:has-text('Informakon'), a:has-text('Informakon')", timeout=30000)
            await botao_informakon.click()
            print("✅ Clicou em 'Informakon'.")
        except Exception as e:
            print(f"❌ Erro ao encontrar o botão 'Informakon': {e}")

            # Se não encontrar, pode estar em iframe
            frames = final_page.frames
            encontrado = False
            for frame in frames:
                try:
                    botao_informakon_iframe = await frame.wait_for_selector("button:has-text('Informakon'), a:has-text('Informakon')", timeout=15000)
                    await botao_informakon_iframe.click(force=True)
                    print("✅ Clicou em 'Informakon' dentro de um iframe.")
                    encontrado = True
                    break
                except Exception as inner_e:
                    print(f"⚠️ Tentativa em um iframe falhou: {inner_e}")
            if not encontrado:
                try:
                    print("🔍 Tentando localizar 'Informakon' via locator com force click...")
                    await final_page.locator("text=Informakon").first.click(force=True)
                    print("✅ Clicou em 'Informakon' com force click.")
                except Exception as final_e:
                    print(f"❌ Não foi possível clicar em 'Informakon': {final_e}")

        # 11. Manter a página aberta para interação manual
        print("✅ Navegação concluída. O navegador permanecerá aberto.")
        await asyncio.sleep(999999)  # Mantém o navegador aberto indefinidamente

# Tratamento para execução dentro do Spyder/Jupyter Notebook
if __name__ == "__main__":
    try:
        loop = asyncio.get_running_loop()
    except RuntimeError:
        loop = None
    if loop and loop.is_running():
        asyncio.create_task(fazer_login())
    else:
        asyncio.run(fazer_login())
