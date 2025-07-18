API_RESPONSE = {
    'user_a': {
        'profile': {
            'name': 'Alice',
            'contact': {'email': 'alice@example.com', 'phone': None}
        },
        'orders': [{'id': 1, 'total': 100}, {'id': 2, 'total': 150}]
    },
    'user_b': {
        'profile': {'name': 'Bob'},
        'orders': []
    },
    'user_c': {
        'profile': None,
        'orders': 123
    }
}

def get_user_details(data, user_id):
    user = data.get(user_id)
    if user is None:
        print(f"User {user_id} not found.")
        return

    profile = user.get("profile", None)

    if profile is None:
        print("Profile not found.")
    else:
        print(f"Name: {profile.get("name", "N/A")}")

        contact_info = profile.get("contact", None)

        if contact_info is None:
            print(f"Contact not found: {profile}")
        else:
            print(f"Email: {contact_info.get("email", "N/A")}")
            print(f"Phone: {contact_info.get("phone", "N/A")}")

    orders = user.get('orders', [])

    try:
        if len(orders) > 0:
            print(f"Number of orders: {len(orders)}")
        else:
            print("No orders found.")
    except TypeError:
        print("Orders is malformed.")
        return

    return True

get_user_details(API_RESPONSE, 'user_a')
print("-" * 20)
get_user_details(API_RESPONSE, 'user_b')
print("-" * 20)
get_user_details(API_RESPONSE, 'user_c')
print("-" * 20)
get_user_details(API_RESPONSE, 'user_d')