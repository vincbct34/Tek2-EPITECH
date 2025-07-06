import socket
from utils import print_debug, setup_logger, LogLevel
import time

class Client:
    def __init__(self, nom_equipe, host="127.0.0.1", port=4242):
        self.nom_equipe = nom_equipe
        self.host = host
        self.port = port
        self.buffer = ""
        self.socket = None
        self.taille_monde = (0, 0)
        self.numero_client = None
        self.connecte = False
        self.logger = setup_logger('CLIENT', LogLevel.DEBUG)

    def connect(self):
        try:
            print_debug(f"🔌 Tentative de connexion à {self.host}:{self.port}")
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.connect((self.host, self.port))
            print_debug(f"✅ Connexion TCP établie")

            self.socket.setblocking(True)
            self.connecte = True
            print_debug(f"✅ Client marqué comme connecté")

            message_bienvenue = self.socket.recv(1024).decode().strip()
            
            if message_bienvenue != "WELCOME":
                raise ConnectionError(f"Attendu 'WELCOME', reçu '{message_bienvenue}'")

            commande_equipe = self.nom_equipe + "\n"
            self.socket.sendall(commande_equipe.encode())
            
            nombre_slots = self.receive_raw()
            
            if nombre_slots == "ko":
                raise ConnectionError(f"Serveur a rejeté le nom d'équipe '{self.nom_equipe}'")
            
            try:
                self.numero_client = int(nombre_slots)
            except ValueError:
                raise ConnectionError(f"Nombre de slots invalide reçu: '{nombre_slots}'")
            
            taille_carte = self.receive_raw()
            self.taille_monde = self.parse_taille_carte(taille_carte)
            
            print_debug(f"🎉 Séquence de connexion terminée avec succès!")
            print_debug(f"├── Équipe: {self.nom_equipe}")
            print_debug(f"├── Numéro client: {self.numero_client}")
            print_debug(f"└── Taille du monde: {self.taille_monde}")
            
        except Exception as e:
            self.connecte = False
            if self.socket:
                try:
                    self.socket.close()
                except:
                    pass
                self.socket = None
            raise

    def receive_raw(self):
        ligne_buffer = ""
        
        while True:
            try:
                data = self.socket.recv(1).decode()
                if not data:
                    raise ConnectionError("Serveur a fermé la connexion pendant la réception")
                
                if data == '\n':
                    break
                    
                ligne_buffer += data
                
            except Exception as e:
                raise
        
        resultat = ligne_buffer.strip()
        return resultat

    def parse_taille_carte(self, taille_str):
        try:
            parties = taille_str.strip().split()
            if len(parties) == 2:
                return (int(parties[0]), int(parties[1]))
        except ValueError:
            pass
        return (0, 0)

    def envoyer_commande(self, commande):
        if not self.connecte or self.socket is None:
            raise ConnectionError("Client n'est pas connecté au serveur.")
        
        try:
            commande_complete = commande + "\n"
            self.logger.debug(f'📤 Envoi commande: {commande}')
            self.socket.sendall(commande_complete.encode())
        except Exception as e:
            self.logger.error(f'❌ Échec envoi commande {commande}: {e}')
            self.connecte = False
            raise
    
    def recevoir(self):
        if not self.connecte:
            raise ConnectionError("Client n'est pas connecté au serveur.")
            
        try:
            while '\n' not in self.buffer:
                data = self.socket.recv(1024).decode()
                if not data:
                    raise ConnectionError("Serveur a fermé la connexion.")
                self.buffer += data

            ligne, self.buffer = self.buffer.split('\n', 1)
            ligne = ligne.strip()
            
            self.logger.debug(f'📥 Réponse reçue: {ligne}')

            if ligne == "dead":
                self.logger.error("💀 Joueur mort!")
                print_debug("💀 Joueur mort!")
                self.connecte = False
                raise SystemExit("⚠️ Joueur est mort. Arrêt...")
            
            if ligne.startswith("message"):
                self.gerer_broadcast(ligne)
                return self.recevoir()
            
            if ligne.startswith("eject"):
                self.gerer_ejection(ligne)
                return self.recevoir()

            return ligne
            
        except Exception as e:
            self.logger.error(f'❌ Échec réception données: {e}')
            self.connecte = False
            raise

    def gerer_broadcast(self, message):
        self.logger.info(f"📻 Broadcast reçu: {message}")
        print_debug(f"📻 Broadcast reçu: {message}")
        
    def gerer_ejection(self, message):
        self.logger.warning(f"💨 Éjecté! {message}")
        print_debug(f"💨 Éjecté! {message}")

    def est_connecte(self):
        return self.connecte and self.socket is not None

    def obtenir_taille_monde(self):
        return self.taille_monde
    
    def get_world_size(self):
        return self.taille_monde
    
    @property
    def team_name(self):
        return self.nom_equipe

    def send_command(self, command):
        return self.envoyer_commande(command)
    
    def receive(self):
        return self.recevoir()
    
    def is_connected(self):
        return self.est_connecte()
    
    def get_client_number(self):
        return self.obtenir_numero_client()
    
    def close(self):
        return self.fermer()

    def obtenir_numero_client(self):
        return self.numero_client

    def fermer(self):
        if self.socket:
            try:
                self.socket.close()
                print_debug("🔌 Connexion fermée.")
            except:
                pass
            finally:
                self.socket = None
                self.connecte = False