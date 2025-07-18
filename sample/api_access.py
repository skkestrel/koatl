API_RESPONSE = {
    'user_a': {
        'profile': {
            'name': 'Alice',
            'contact': {'email': 'alice@example.com', 'phone': None}
        },
        'orders': [{'id': 1, 'total': 100}, {'id': 2, 'total': 150}]
    },
    'user_b': {
        'profile': {'name': 'Bob'}, # 'contact' is missing
        'orders': [] # 'orders' is an empty list
    },
    'user_c': {
        'profile': None # 'profile' object itself is None
    }
}

def get_user_details(data, user_id):
    print(f"--- Processing user: {user_id} ---")

    user = data.get(user_id)
    if user is None:
        print("❌ User not found.")
        return

    try:
        profile = user.get('profile')
        if profile is not None:
            name = profile.get('name', 'N/A')
            print(f"👤 Name: {name}")

            contact_info = profile.get('contact')
            if contact_info is not None:
                email = contact_info.get('email', 'No email provided')
                print(f"📧 Email: {email}")
            else:
                print("📭 No contact info available.")
        else:
            print("📄 No profile available.")
    except AttributeError:
        print("❌ Error: 'profile' data is malformed.")

    try:
        orders = user.get('orders')
        if orders is not None and len(orders) > 0:
            order_count = len(orders)
            print(f"🛒 Found {order_count} order(s).")
        else:
            print("🚫 No orders found.")
    except TypeError:
        print("❌ Error: 'orders' data is not a list.")


get_user_details(API_RESPONSE, 'user_a')
print("-" * 20)
get_user_details(API_RESPONSE, 'user_b')
print("-" * 20)
get_user_details(API_RESPONSE, 'user_c')
print("-" * 20)
get_user_details(API_RESPONSE, 'user_d') # A user who doesn't exist