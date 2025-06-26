
import pandas as pd
from ofxparse import OfxParser

def parse_ofx_file(path):
    """
    Parses an OFX file and returns a pandas DataFrame.

    Args:
        path (str): The full path to the .ofx file.

    Returns:
        pandas.DataFrame: A DataFrame containing the transaction data.
    """
    with open(path, 'rb') as f:
        ofx = OfxParser.parse(f)

    account = ofx.account
    statement = account.statement
    transactions = statement.transactions

    data = []
    for t in transactions:
        data.append({
            'date': t.date,
            'payee': t.payee,
            'type': t.type,
            'amount': t.amount,
            'id': t.id,
            'memo': t.memo
        })

    return pd.DataFrame(data)
