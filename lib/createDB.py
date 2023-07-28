import sqlite3
    
conn = sqlite3.connect('database/gas_data.db')
cursor = conn.cursor()

drop_table_query = 'DROP TABLE IF EXISTS gas_prices;'
cursor.execute(drop_table_query)
conn.commit()

cursor=conn.cursor()
print('Database Cleared')

create_table_query = '''
CREATE TABLE gas_prices (
    padd varchar(25),
    duoarea varchar(25),
    period date,
    product varchar(50),
    product_name varchar(50),
    series_description varchar(50),
    price float,
    units varchar(25)
);
'''
cursor.execute(create_table_query)

conn.commit()
conn.close()
print('Database Created')
